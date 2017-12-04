#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/weak.h>
#include <caml/custom.h>
#include "caml/finalise.h"
#include "caml/globroots.h"
#include "caml/roots.h"
#include "caml/stacks.h"

/******************************************************************************/
/**************************** Global configuration ****************************/
/******************************************************************************/
#ifndef __amd64__
#error "Not yet implemented on this architecture"
#endif

#define FQN(fun) lhamt_##fun##_32k
#define FQCN(fun) caml_lhamt_##fun##_32k

#define SIZE 32
#define SHIFT 5
#define DEPTH ((sizeof(value) * 8 + SHIFT - 1) / SHIFT)

#define Empty Val_unit
#define Item 0
#define Bucket 1
#define Knot 2
#define Node 3

volatile register value *custom_young_ptr asm ("r15");

/******************************************************************************/
/****************************** Accessor macros  ******************************/
/******************************************************************************/

#define Hd(t) Hd_val(t)
#define Size(t) Wosize_val(t)
#define Tag(t) Tag_val(t)

#define Bitmap(t) Field(t, 0)

#define Val(t) Val_long(t)
#define Long(t) Unsigned_long_val(t)

/******************************************************************************/
/****************************** Helper functions ******************************/
/******************************************************************************/

#define Popcnt __builtin_popcountll
#define Bzhi __builtin_ia32_bzhi_di
#define Tzcnt __builtin_ctzll
#define Lzcnt __builtin_clzll
#define Blsr(x) ((x) & ((x) - 1))
#define Bextr(x, y, z) __builtin_ia32_bextr_u64((x), (y) | ((z) << 8))

#define Select(b, i) ((uintnat) (b) << (i))
/* Computes the real position of an element in an Array Map */
#define Map(bitmap, i) Popcnt(Bzhi(bitmap, i))
#define Store(array, bitmap, i) Field(array, Map(bitmap, i))
/* Computes the position according to the hash and the current level */
#define Index(hash, level) Bextr(hash, level, SHIFT)

static void knot_pp (value t)
{
  uintnat bitmap = Bitmap(t);
  printf("[| ");
  for (uintnat i = 0, j = 1; i < SIZE; i += 1) {
    if (bitmap & Select(2, i)) {
      value a = Field(t, j); j += 1;
      printf("%d ", Tag(a) + 1);
    } else {
      printf("0 ");
    }
  }
  printf("|]\n");
}

static void node_pp (value t)
{
  printf("[| ");
  for (uintnat i = 0; i < SIZE; i += 1) {
    value v = Field(t, i);
    printf("%d ", (v == Val_unit)?0:Tag(v)+1);
  }
  printf("|]\n");
}

/******************************************************************************/
/******************************** Acceleration ********************************/
/******************************************************************************/

__attribute__((target ("bmi2")))
CAMLprim CAMLweakdef uintnat tzcnt (uintnat i)
{
  return Tzcnt(i);
}

__attribute__((target ("bmi2", "popcnt")))
CAMLprim value FQN(knot_get) (value t, uintnat i)
{
  return Store(t, Bitmap(t), i + 1);
}

CAMLprim value FQN(knot_make) (uintnat i, value a)
{
  custom_young_ptr -= 3;
  if (custom_young_ptr < caml_young_trigger) {
    return Empty;
  }
  value knot = (value) (custom_young_ptr + 1);
  Hd(knot) = Make_header(2, Knot, Caml_black);
  Bitmap(knot) = Select(2, i) | Val_unit;
  Field(knot, 1) = a;
  return knot;
}

__attribute__((target ("bmi", "bmi2", "popcnt")))
CAMLprim value FQN(knot_add) (value t, uintnat i, value a)
{
  if (custom_young_ptr - (SIZE + 2) < caml_young_trigger) {
    return Empty;
  }
  uintnat size = Size(t);
  uintnat bitmap = Bitmap(t) | Select(2, i);
  if (size > 3 * SIZE / 4) {
    custom_young_ptr -= (SIZE + 2);
    value node = (value) (custom_young_ptr + 1);
    Hd(node) = Make_header(SIZE + 1, Node, Caml_black);
    Field(node, SIZE) = bitmap;
    for (uintnat i = 1, b = Long(Bitmap(t)); i < size; i += 1) {
      uintnat j = Tzcnt(b); b = Blsr(b);
      Field(node, j) = Field(t, i);
    }
    Field(node, i) = a;
    for (uintnat b = Long(bitmap) ^ 0xffffffff; b > 0; b = Blsr(b)) {
      uintnat j = Tzcnt(b);
      Field(node, j) = Empty;
    }
    return node;
  } else {
    custom_young_ptr -= (size + 2);
    value knot = (value) (custom_young_ptr + 1);
    Hd(knot) = Make_header(size + 1, Knot, Caml_black);
    Bitmap(knot) = bitmap;
    uintnat j = Map(bitmap, i + 1);
    Field(knot, j) = a;
    for (uintnat i = 1; i < j; i += 1) {
      Field(knot, i) = Field(t, i);
    }
    for (uintnat i = j; i < size; i += 1) {
      Field(knot, i + 1) = Field(t, i);
    }
    return knot;
  }
}

__attribute__((target ("bmi2", "popcnt")))
CAMLprim value FQN(knot_set) (value t, uintnat i, value a)
{
  uintnat size = Size(t);
  custom_young_ptr -= (size + 1);
  if (custom_young_ptr < caml_young_trigger) {
    return Empty;
  }
  value knot = (value) (custom_young_ptr + 1);
  Hd(knot) = Make_header(size, Knot, Caml_black);
  Bitmap(knot) = Bitmap(t);
  for (uintnat i = 1; i < size; i += 1) {
    Field(knot, i) = Field(t, i);
  }
  Store(knot, Bitmap(knot), i + 1) = a;
  return knot;
}

__attribute__((target ("bmi2", "popcnt")))
CAMLprim value FQN(knot_seti) (value t, uintnat i, value a)
{
  uintnat size = Size(t);
  if (size == 2) {
    return a;
  } else {
    return FQN(knot_set)(t, i, a);
  }
}

__attribute__((target ("bmi2", "popcnt")))
CAMLprim value FQN(knot_remove) (value t, uintnat i)
{
  CAMLparam1(t);
  uintnat size = Size(t);
  if (size == 2) {
    CAMLreturn(Empty);
  }
  value bitmap = Bitmap(t) ^ Select(2, i);
  if (size == 3) {
    uintnat j = 3 - Map(Bitmap(t), i + 1);
    t = Field(t, j);
    if (Tag(t) > Bucket) {
      value knot = caml_alloc_small(2, Knot);
      Bitmap(knot) = bitmap;
      Field(knot, 1) = t;
      CAMLreturn(knot);
    }
    CAMLreturn(t);
  }
  value knot = caml_alloc_small(size - 1, Knot);
  Bitmap(knot) = bitmap;
  uintnat j = Map(Bitmap(t), i + 1);
  for (uintnat i = 1; i < j; i += 1) {
    Field(knot, i) = Field(t, i);
  }
  for (uintnat i = j; i < size - 1; i += 1) {
    Field(knot, i) = Field(t, i + 1);
  }
  CAMLreturn(knot);
}

CAMLprim value FQN(node_set) (value t, uintnat i, value a)
{
  custom_young_ptr -= (SIZE + 2);
  if (custom_young_ptr < caml_young_trigger) {
    return Empty;
  }
  value node = (value) (custom_young_ptr + 1);
  Hd(node) = Make_header(SIZE + 1, Node, Caml_black);
  Field(node, SIZE) = Field(t, SIZE) | Select(2, i);
  for (uintnat i = 0; i < SIZE; i += 1) {
    Field(node, i) = Field(t, i);
  }
  Field(node, i) = a;
  return node;
}

__attribute__((target ("bmi", "bmi2", "popcnt")))
CAMLprim value FQN(node_remove) (value t, uintnat i)
{
  if (custom_young_ptr - SIZE - 2 < caml_young_trigger) {
    return Empty;
  }
  value bitmap = Field(t, SIZE) ^ Select(2, i);
  uintnat size = Popcnt(bitmap) - 1;
  if (size > 3 * SIZE / 4) {
    custom_young_ptr -= (SIZE + 2);
    value node = (value) (custom_young_ptr + 1);
    Hd(node) = Make_header(SIZE + 1, Node, Caml_black);
    Field(node, SIZE) = bitmap;
    for (uintnat i = 0; i < SIZE; i += 1) {
      Field(node, i) = Field(t, i);
    }
    Field(node, i) = Empty;
    return node;
  } else {
    custom_young_ptr -= (size + 2);
    value knot = (value) (custom_young_ptr + 1);
    Hd(knot) = Make_header(size + 1, Knot, Caml_black);
    Bitmap(knot) = bitmap;
    bitmap = Long(bitmap);
    for (uintnat i = 1; i < size + 1; i += 1) {
      Field(knot, i) = Field(t, Tzcnt(bitmap));
      bitmap = Blsr(bitmap);
    }
    assert(bitmap == 0);
    return knot;
  }
}

/******************************************************************************/
/******************************** Custom stack ********************************/
/******************************************************************************/

struct node {
  value array[SIZE];
  uintnat bitmap;
};

static void draft_pp (struct node *t)
{
  uintnat bitmap = t->bitmap;
  printf("[| ");
  for (uintnat i = 0; i < SIZE; i += 1) {
    if (bitmap & Select(1, i)) {
      value a = Field(t, i);
      printf("%d ", Tag(a) + 1);
    } else {
      printf("0 ");
    }
  }
  printf("|]\n");
}

struct stack {
  intnat depth;
  struct node node[DEPTH];
};

struct heap {
  uintnat size;
  uintnat load;
  void *block;
  struct stack *stack;
};
static struct heap heap = {0, 0, NULL, NULL};
void (*old_scan_roots_hook) (scanning_action f) = NULL;

void scan_heap_roots (scanning_action f)
{
  for (uintnat i = 0; i < heap.load; i += 1) {
    struct stack *stack = heap.stack + i;
    for (intnat j = 0; j <= stack->depth; j += 1) {
      struct node *node = stack->node + j;
      for (uintnat bitmap = node->bitmap; bitmap != 0; bitmap = Blsr(bitmap)) {
        value *t = node->array + Tzcnt(bitmap);
        f(*t, t);
      }
    }
  }
}

CAMLprim void FQN(stack_init) (void)
{
  void *raw, *ptr;
  if (heap.block == NULL) {
    raw = malloc(sizeof(struct stack) + sizeof(value) - 1);
    ptr = (void *) (((uintnat) raw + sizeof(value) - 1) ^ (sizeof(value) - 1));
    heap.size = 1;
    heap.load = 0;
    heap.block = raw;
    heap.stack = ptr;
    old_scan_roots_hook = caml_scan_roots_hook;
    caml_scan_roots_hook = &scan_heap_roots;
  }
}

CAMLprim uintnat FQN(stack_make) (void)
{
  void *raw, *ptr;
  if (heap.load == heap.size) {
    heap.size *= 2;
    raw = malloc(sizeof(struct stack) * heap.size + sizeof(value) - 1);
    ptr = (void *) (((uintnat) raw + sizeof(value) - 1) ^ (sizeof(value) - 1));
    memcpy(ptr, heap.stack, sizeof(struct stack) * heap.load);
    free(heap.block);
    heap.block = raw;
    heap.stack = ptr;
  }
  struct stack * stack = heap.stack + heap.load;
  stack->depth = -1;
  heap.load += 1;
  return heap.load - 1;
}

CAMLprim void FQN(stack_push) (uintnat q)
{
  struct stack *stack = heap.stack + q;
  stack->depth += 1;
  struct node *node = stack->node + stack->depth;
  node->bitmap = 0;
}

CAMLprim void FQN(stack_push_node) (uintnat q, value t)
{
  struct stack *stack = heap.stack + q;
  stack->depth += 1;
  struct node *node = stack->node + stack->depth;
  node->bitmap = Long(Field(t, SIZE));
  for (uintnat i = 0; i < SIZE; i += 1) {
    node->array[i] = Field(t, i);
  }
}

CAMLprim void FQN(stack_push_knot) (uintnat q, value t)
{
  struct stack *stack = heap.stack + q;
  stack->depth += 1;
  struct node *node = stack->node + stack->depth;
  node->bitmap = Long(Bitmap(t));
  uintnat i = 1, bitmap = node->bitmap;
  while (bitmap != 0) {
    node->array[Tzcnt(bitmap)] = Field(t, i);
    i += 1;
    bitmap = Blsr(bitmap);
  }
}

CAMLprim value FQN(stack_get) (uintnat q, uintnat i)
{
  struct stack *stack = heap.stack + q;
  struct node *node = stack->node + stack->depth;
  return node->array[i];
}

CAMLprim void FQN(stack_set) (uintnat q, uintnat i, value t)
{
  struct stack *stack = heap.stack + q;
  struct node *node = stack->node + stack->depth;
  node->array[i] = t;
  node->bitmap |= Select(1, i);
}

CAMLprim void FQN(stack_clear) (uintnat q, uintnat i)
{
  struct stack *stack = heap.stack + q;
  struct node *node = stack->node + stack->depth;
  node->bitmap &= ~Select(1, i);
}

CAMLprim value FQN(stack_pop) (uintnat q)
{
  struct stack *stack = heap.stack + q;
  struct node *draft = stack->node + stack->depth;
  uintnat bitmap = draft->bitmap, size = Popcnt(bitmap);
  value t;
  if (size > 3 * SIZE / 4) {
    value node = caml_alloc_small(SIZE + 1, Node);
    Field(node, SIZE) = Val(bitmap);
    for (uintnat b = bitmap; b != 0; b = Blsr(b)) {
      uintnat j = Tzcnt(b);
      Field(node, j) = draft->array[j];
    }
    for (uintnat b = bitmap ^ 0xffffffff; b != 0; b = Blsr(b)) {
      uintnat j = Tzcnt(b);
      Field(node, j) = Empty;
    }
    t = node;
  } else if (size > 1) {
    value knot = caml_alloc_small(size + 1, Knot);
    Bitmap(knot) = Val(bitmap);
    uintnat i = 1;
    while (bitmap != 0) {
      Field(knot, i) = draft->array[Tzcnt(bitmap)];
      i += 1;
      bitmap = Blsr(bitmap);
    }
    t = knot;
  } else if (size == 1) {
    uintnat j = Tzcnt(bitmap);
    t = draft->array[j];
    if (Tag(t) > Bucket) {
      t = caml_alloc_small(2, Knot);
      Bitmap(t) = Select(2, j) | Val_unit;
      Field(t, 1) = draft->array[j];
    }
  } else {
    t = Empty;
  }
  stack->depth -= 1;
  if (stack->depth < 0) {
    heap.load -= 1;
  }
  return t;
}

/******************************************************************************/
/********************************* Caml Stubs *********************************/
/******************************************************************************/

CAMLprim CAMLweakdef value caml_tzcnt (uintnat i)
{
  return tzcnt(Long(i));
}

CAMLprim value FQCN(knot_get) (value t, value i)
{
  return FQN(knot_get)(t, Long(i));
}

CAMLprim value FQCN(knot_make) (value i, value t)
{
  return FQN(knot_make)(Long(i), t);
}

CAMLprim value FQCN(knot_add) (value t, value i, value a)
{
  return FQN(knot_add)(t, Long(i), a);
}

CAMLprim value FQCN(knot_set) (value t, value i, value a)
{
  return FQN(knot_set)(t, Long(i), a);
}

CAMLprim value FQCN(knot_seti) (value t, value i, value a)
{
  return FQN(knot_seti)(t, Long(i), a);
}

CAMLprim value FQCN(knot_remove) (value t, value i)
{
  return FQN(knot_remove)(t, Long(i));
}

CAMLprim value FQCN(node_set) (value t, value i, value a)
{
  return FQN(node_set)(t, Long(i), a);
}

CAMLprim value FQCN(node_remove) (value t, value i)
{
  return FQN(node_remove)(t, Long(i));
}

CAMLprim value FQCN(stack_make) (void)
{
  return Val(FQN(stack_make)());
}

CAMLprim void FQCN(stack_push) (value q)
{
  FQN(stack_push)(Long(q));
}

CAMLprim void FQCN(stack_push_node) (value q, value t)
{
  FQN(stack_push_node)(Long(q), t);
}

CAMLprim void FQCN(stack_push_knot) (value q, value t)
{
  FQN(stack_push_knot)(Long(q), t);
}

CAMLprim value FQCN(stack_get) (value q, value i)
{
  return FQN(stack_get)(Long(q), Long(i));
}

CAMLprim void FQCN(stack_set) (value q, value i, value t)
{
  FQN(stack_set)(Long(q), Long(i), t);
}

CAMLprim void FQCN(stack_clear) (value q, value i)
{
  FQN(stack_clear)(Long(q), Long(i));
}

CAMLprim value FQCN(stack_pop) (value q)
{
  return FQN(stack_pop)(Long(q));
}
