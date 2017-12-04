external tzcnt : int -> int
  = "caml_tzcnt" "tzcnt" [@@untagged] [@@noalloc]

module type HashedType = sig
  type t
  val equal   : t -> t -> bool
  val hash    : t      -> int
end

module type S = sig
  type key
  type 'a t

  val empty    : 'a t

  val add      : key -> 'a -> 'a t -> 'a t
  val remove   : key ->       'a t -> 'a t

  val mem      : key ->       'a t -> bool
  val find     : key ->       'a t -> 'a

  val bindings : 'a t              -> (key * 'a) list

  (*val merge :
    (key -> 'a option -> 'b option-> 'c option) -> 'a t -> 'b t -> 'c t*)
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val join : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
end

module Make(Key : HashedType) : S with type key = Key.t = struct
  let size = 32
  let shift = 5
  let prefix = 0b11111
  let full = 0xffffffff

  type key = Key.t
  type abstract = unit

  module Bucket = struct
    type 'a t = (key * 'a) list

    let empty : 'a t = []
    let add key value t : 'a t =
      (key, value) :: (List.filter (fun (k, _) -> not (Key.equal key k)) t)
    let remove key t : 'a t =
      List.filter (fun (k, _) -> not (Key.equal key k)) t
    let mem key t : bool = List.mem_assoc key t
    let find key t : 'a = List.assoc key t
    let fold f t b : 'b = List.fold_left (fun b (k, v) -> f k v b) b t
    let merge f b b' : 'a t =
      List.fold_left
        (fun m (k, v) ->
           if List.mem_assoc k b then
             m
           else
             match f k None (Some v) with
             | None -> m
             | Some v' -> (k, v') :: m)
        (List.fold_left
           (fun m (k, v) ->
              try
                match f k (Some v) (Some (List.assoc k b')) with
                | None -> m
                | Some v' -> (k, v') :: m
              with Not_found ->
                match f k (Some v) None with
                | None -> m
                | Some v' -> (k, v') :: m)
           [] b)
        b'
    let union f b b' : 'a t =
      List.fold_left
        (fun u ((k, v) as a) ->
           if List.mem_assoc k b then u else a :: u)
        (List.fold_left
           (fun u ((k, v) as a) ->
              try
                match f k v (List.assoc k b') with
                | None -> u
                | Some v' -> (k, v') :: u
              with Not_found -> a :: u)
           [] b)
        b'
    let join f b b' : 'a t =
      List.fold_left
        (fun j (k, v) ->
           try
             match f k v (List.assoc k b') with
             | None -> j
             | Some v' -> (k, v') :: j
           with Not_found -> j)
        [] b
  end [@@inlined]

  type 'a t =
    | Empty
    | Item   of key * 'a * int
    | Bucket of 'a Bucket.t * int
    | Knot   of int * abstract
    | Node   of abstract

  external call_gc : unit -> unit
    = "caml_gc_dispatch"

  module Knot = struct
    external length : 'a t -> int
      = "%array_length" [@@noalloc]
    external (@) : 'a t -> int -> 'a t
      = "%obj_field" [@@noalloc]
    let fold f b t : 'b =
      let b = ref b in
      for i = 1 to length t - 1 do
        b := f !b (t@i)
      done;
      !b [@@inline]

    let select i = 1 lsl i [@@inline]

    let mem b i : bool = (b land select i) != 0 [@@inlined]

    external get : 'a t -> (int [@untagged]) -> 'a t
      = "caml_lhamt_knot_get_32k" "lhamt_knot_get_32k" [@@noalloc]

    external make : (int [@untagged]) -> 'a t -> 'a t
      = "caml_lhamt_knot_make_32k" "lhamt_knot_make_32k" [@@noalloc]
    let make i t : 'a t = match make i t with
      | Empty -> call_gc (); make i t
      | t' -> t' [@@inline]

    external add : 'a t -> (int [@untagged]) -> 'a t -> 'a t
      = "caml_lhamt_knot_add_32k" "lhamt_knot_add_32k" [@@noalloc]
    let add t i t' : 'a t = match add t i t' with
      | Empty -> call_gc (); add t i t'
      | t'' -> t'' [@@inline]

    external set : 'a t -> (int [@untagged]) -> 'a t -> 'a t
      = "caml_lhamt_knot_set_32k" "lhamt_knot_set_32k" [@@noalloc]
    let set t i t' : 'a t = match set t i t' with
      | Empty -> call_gc (); set t i t'
      | t'' -> t'' [@@inline]

    external seti : 'a t -> (int [@untagged]) -> 'a t -> 'a t
      = "caml_lhamt_knot_seti_32k" "lhamt_knot_seti_32k" [@@noalloc]
    let seti t i t' : 'a t = match seti t i t' with
      | Empty -> call_gc (); seti t i t'
      | t'' -> t'' [@@inline]

    external remove : 'a t -> (int [@untagged]) -> 'a t
      = "caml_lhamt_knot_remove_32k" "lhamt_knot_remove_32k"
  end

  module Node = struct
    external get : 'a t -> int -> 'a t
      = "%obj_field" [@@noalloc]

    external set : 'a t -> (int [@untagged]) -> 'a t -> 'a t
      = "caml_lhamt_node_set_32k" "lhamt_node_set_32k" [@@noalloc]
    let set t i t' : 'a t = match set t i t' with
      | Empty -> call_gc (); set t i t'
      | t'' -> t'' [@@inline]

    external remove : 'a t -> (int [@untagged]) -> 'a t
      = "caml_lhamt_node_remove_32k" "lhamt_node_remove_32k" [@@noalloc]
    let remove t i : 'a t = match remove t i with
      | Empty -> call_gc (); remove t i
      | t' -> t' [@@inline]

    let fold f b t : 'b =
      let b = ref b in
      for i = 0 to size - 1 do
        b := f !b (get t i)
      done;
      !b [@@inline]
  end

  let empty = Empty

  let rec recadd d k a h t = match t with
    | Empty ->
      Item (k, a, h)
    | Item (k', a', h') ->
      if h = h' then
        if Key.equal k k' then
          Item (k, a, h)
        else
          Bucket (Bucket.add k' a' [(k, a)], h)
      else
        let i' = prefix land h' lsr d in
        recadd d k a h (Knot.make i' t)
    | Bucket (l, h') ->
      if h = h' then
        Bucket (Bucket.add k a l, h)
      else
        let i' = prefix land h' lsr d in
        recadd d k a h (Knot.make i' t)
    | Knot (b', _) ->
      let i = prefix land h lsr d in
      if Knot.mem b' i then
        Knot.set t i (recadd (d + shift) k a h (Knot.get t i))
      else Knot.add t i (Item (k, a, h))
    | Node _ ->
      let i = prefix land h lsr d in
      Node.set t i (recadd (d + shift) k a h (Node.get t i))
  let add k a t = recadd 0 k a (Key.hash k) t

  let rec recremove d k h t = match t with
    | Empty -> Empty
    | Item (k', a', h') ->
      if h = h' && Key.equal k k' then Empty else t
    | Bucket (l', h') ->
      if h = h' then
        match Bucket.remove k l' with
        | [] -> Empty
        | l'' when l' == l'' -> t
        | l'' -> Bucket (l'', h')
      else t
    | Knot (b', _) ->
      let i = prefix land h lsr d in
      if Knot.mem b' i then
        let t' = Knot.get t i in
        let t'' = recremove (d + shift) k h t' in
        match t'' with
        | t'' when t'' == t' -> t
        | Empty -> Knot.remove t i
        | Item _ | Bucket _ -> Knot.seti t i t''
        | t'' -> Knot.set t i t''
      else t
    | Node _ ->
      let i = prefix land h lsr d in
      let t' = Node.get t i in
      match recremove (d + shift) k h t' with
      | t'' when t'' == t' -> t
      | Empty -> Node.remove t i
      | t'' -> Node.set t i t''
  let remove k t = recremove 0 k (Key.hash k) t

  let rec recmem d k h t = match t with
    | Empty -> false
    | Item (k', _, h') -> if h = h' then Key.equal k k' else false
    | Bucket (l', h') -> if h = h' then Bucket.mem k l' else false
    | Knot (b', _) ->
      let i = prefix land h lsr d in
      if Knot.mem b' i then recmem (d + shift) k h (Knot.get t i)
      else false
    | Node _ ->
      let i = prefix land h lsr d in
      recmem (d + shift) k h (Node.get t i)
  let mem k t = recmem 0 k (Key.hash k) t

  let rec recfind d k h t = match t with
    | Empty -> raise Not_found
    | Item (k', a', h') ->
      if h = h' && Key.equal k k' then a' else raise Not_found
    | Bucket (l', h') -> if h = h' then Bucket.find k l' else raise Not_found
    | Knot (b', _) ->
      let i = prefix land h lsr d in
      if Knot.mem b' i then recfind (d + shift) k h (Knot.get t i)
      else raise Not_found
    | Node _ ->
      let i = prefix land h lsr d in
      recfind (d + shift) k h (Node.get t i)
  let find k t = recfind 0 k(Key.hash k) t

  let rec recbindings b l = match l with
    | [] -> b
    | t :: l ->
      match t with
      | Empty -> recbindings b l
      | Item (k', a', _) -> recbindings ((k', a') :: b) l
      | Bucket (l', _) ->
        recbindings (Bucket.fold (fun k v l' -> (k, v) :: l') l' b) l
      | Knot _ ->
        recbindings b (Knot.fold (fun l t -> t :: l) l t)
      | Node _ ->
        recbindings b (Node.fold (fun l t -> t :: l) l t)
  and bindings m = recbindings [] [m]


  module Stack = struct
    type 'a stack = private int

    external init : unit -> unit
      = "lhamt_stack_init_32k" [@@noalloc]
    let () = init ()

    external make : unit -> ('a t stack [@untagged])
      = "caml_lhamt_stack_make_32k" "lhamt_stack_make_32k" [@@noalloc]

    external push : ('a t stack [@untagged]) -> unit
      = "caml_lhamt_stack_push_32k" "lhamt_stack_push_32k" [@@noalloc]

    external push_node : ('a t stack [@untagged]) -> 'a t -> unit
      = "caml_lhamt_stack_push_node_32k" "lhamt_stack_push_node_32k" [@@noalloc]

    external push_knot : ('a t stack [@untagged]) -> 'a t -> unit
      = "caml_lhamt_stack_push_knot_32k" "lhamt_stack_push_knot_32k" [@@noalloc]

    external get : ('a t stack [@untagged]) -> (int [@untagged]) -> 'a t
      = "caml_lhamt_stack_get_32k" "lhamt_stack_get_32k" [@@noalloc]

    external set : ('a t stack [@untagged]) -> (int [@untagged]) -> 'a t -> unit
      = "caml_lhamt_stack_set_32k" "lhamt_stack_set_32k" [@@noalloc]

    external clear : ('a t stack [@untagged]) -> (int [@untagged]) -> unit
      = "caml_lhamt_stack_clear_32k" "lhamt_stack_clear_32k" [@@noalloc]

    external pop : ('a t stack [@untagged]) -> 'a t
      = "caml_lhamt_stack_pop_32k" "lhamt_stack_pop_32k"
  end

  (*let rec recmerge q d f t t' = match t, t' with
    | Empty, Empty -> Empty
    | Empty, Item (k', a', h') ->
      begin
        match f k' None (Some a') with
        | None -> Empty
        | Some a'' -> Item (k', a'', h')
      end
    | Item (k, a, h), Empty ->
      begin
        match f k (Some a) None with
        | None -> Empty
        | Some a'' -> Item (k, a'', h)
      end
    | Empty, Bucket (b', h') ->
      begin
        match Bucket.merge f [] b' with
        | [] -> Empty
        | b'' -> Bucket (b'', h')
      end
    | Bucket (b, h), Empty ->
      begin
        match Bucket.merge f b [] with
        | [] -> Empty
        | b' -> Bucket (b', h)
      end
    | Item (k, a, h), Item (k', a', h') ->
      if h = h' && Key.equal k k' then
        match f k (Some a) (Some a') with
        | None -> Empty
        | Some a'' -> Item (k, a'', h)
      else
        begin
          match f k (Some a) None, f k' None (Some a') with
          | None, None -> Empty
          | Some a'', None -> Item (k, a'', h)
          | None, Some a''' -> Item (k', a''', h')
          | Some a'', Some a''' -> recadd d k a'' h (Item (k', a''', h'))
        end
    | Item (k, a, h), Bucket (b', h') ->
      if h = h' && Bucket.mem k b' then
        match Bucket.merge f [(k, a)] b' with
        | [] -> Empty
        | b'' -> Bucket (b'', h)
      else
        begin
          match f k (Some a) None, Bucket.merge f [] b' with
          | None, [] -> Empty
          | Some a', [] -> Item (k, a', h)
          | None, b'' -> Bucket (b'', h')
          | Some a', b'' -> recadd d k a' h (Bucket (b'', h'))
        end
    | Bucket (b, h), Item (k', a', h') ->
      if h = h' && Bucket.mem k' b then
        match Bucket.merge f b [(k', a')] with
        | [] -> Empty
        | b' -> Bucket (b', h)
      else
        begin
          match Bucket.merge f b [], f k' None (Some a') with
          | [], None -> Empty
          | b', None -> Bucket (b', h)
          | [], Some a'' -> Item (k', a'', h')
          | b', Some a'' -> recadd d k' a'' h' (Bucket (b', h))
        end
    | Bucket (b, h), Bucket (b', h') ->
      if h = h' then
        match Bucket.merge f b b' with
        | [] -> Empty
        | b'' -> Bucket (b'', h)
      else
        let i' = prefix land h' lsr d in
        recmerge q d f t (Knot.make i' t')
    | Empty, Knot (b', _) ->
      Stack.push q;
      let m = ref b' in
      let s = Knot.length t' in
      for i = 1 to s - 1 do
        let j' = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f Empty (Node.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q j' t''
      done;
      Stack.pop q
    | Knot (b, _), Empty ->
      Stack.push q;
      let m = ref b in
      let s = Knot.length t in
      for i = 1 to s - 1 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f (Node.get t i) Empty with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      Stack.pop q
    | (Item (_, _, h) | Bucket (_, h)), Knot (b', _) ->
      Stack.push q;
      let i = prefix land h lsr d in
      let m = ref (b' lxor Knot.select i) in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f Empty (Knot.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      begin
        match recmerge q (d + shift) f t (Knot.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      end;
      Stack.pop q
    | Knot (b, _), (Item (_, _, h') | Bucket (_, h')) ->
      Stack.push q;
      let i' = prefix land h' lsr d in
      let m = ref (b lxor Knot.select i') in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f (Knot.get t j) Empty with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      begin
        match recmerge q (d + shift) f (Knot.get t i') t' with
        | Empty -> ()
        | t'' -> Stack.set q i' t''
      end;
      Stack.pop q
    | Knot (b, _), Knot (b', _) ->
      Stack.push q;
      let m = ref (b land b') in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f (Knot.get t j) (Knot.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      let m = ref (b land (lnot b')) in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f (Knot.get t j) Empty with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      let m = ref ((lnot b) land b') in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f Empty (Knot.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      Stack.pop q
    | Empty, Node _ ->
      Stack.push q;
      for i = 0 to size - 1 do
        match recmerge q (d + shift) f Empty (Node.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      Stack.pop q
    | Node _, Empty ->
      Stack.push q;
      for i = 0 to size - 1 do
        match recmerge q (d + shift) f (Node.get t i) Empty with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      Stack.pop q
    | (Item (_, _, h) | Bucket (_, h)), Node _ ->
      Stack.push q;
      let j = prefix land h lsr d in
      for i = 0 to j - 1 do
        match recmerge q (d + shift) f Empty (Node.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      begin
        match recmerge q (d + shift) f t (Node.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      end;
      for i = j + 1 to size - 1 do
        match recmerge q (d + shift) f Empty (Node.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      Stack.pop q
    | Node _, (Item (_, _, h') | Bucket (_, h')) ->
      Stack.push q;
      let j' = prefix land h' lsr d in
      for i = 0 to j' - 1 do
        match recmerge q (d + shift) f (Node.get t i) Empty with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      begin
        match recmerge q (d + shift) f (Node.get t j') t' with
        | Empty -> ()
        | t'' -> Stack.set q j' t''
      end;
      for i = j' + 1 to size - 1 do
        match recmerge q (d + shift) f (Node.get t i) Empty with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      Stack.pop q
    | Knot (b, _), Node _ ->
      Stack.push q;
      let m = ref b in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f (Knot.get t j) (Node.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      let m = ref (b lxor full) in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f Empty (Node.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      Stack.pop q
    | Node _, Knot (b', _) ->
      Stack.push q;
      let m = ref b' in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f (Node.get t j) (Knot.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      let m = ref (full lxor b') in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recmerge q (d + shift) f (Node.get t j) Empty with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      Stack.pop q
    | Node _, Node _ ->
      Stack.push q;
      for i = 0 to size - 1 do
        match recmerge q (d + shift) f (Node.get t i) (Node.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      Stack.pop q
  let merge f t t' = recmerge (Stack.make ()) 0 f t t'*)

  let rec recunion q d f t t' = match t, t' with
    | _, _ when t == t' -> t
    | Empty, _ -> t'
    | _, Empty -> t
    | Item (k, a, h), Item (k', a', h') ->
      if h = h' && Key.equal k k' then
        match f k a a' with
        | None -> Empty
        | Some a'' -> Item (k, a'', h)
      else
        let i' = prefix land h' lsr d in
        recadd d k a h (Knot.make i' t')
    | Bucket (b, h), Item (k', a', h') ->
      if h = h' && Bucket.mem k' b then
        Bucket (Bucket.union f b [(k', a')], h)
      else
        let i = prefix land h' lsr d in
        recadd d k' a' h' (Knot.make i t)
    | Bucket (b, h), Bucket (b', h') ->
      if h = h' then
        Bucket (Bucket.union f b b', h)
      else
        let i' = prefix land h' lsr d in
        recunion q d f t (Knot.make i' t')
    | Knot (b, _), (Item (_, _, h') | Bucket (_, h')) ->
      let i' = prefix land h' lsr d in
      if Knot.mem b i' then
        match recunion q (d + shift) f (Knot.get t i') t' with
        | Empty -> Knot.remove t i'
        | t'' -> Knot.set t i' t''
      else Knot.add t i' t'
    | Knot (b, _), Knot (b', _) ->
      Stack.push_knot q t;
      let m' = ref b' in
      let s' = Knot.length t' in
      for i' = 1 to s' - 1 do
        let j = tzcnt !m' in m' := !m' land (!m' - 1);
        if Knot.mem b j then
          match recunion q (d + shift) f (Stack.get q j) (Node.get t' i') with
          | Empty -> Stack.clear q j
          | t'' -> Stack.set q j t''
        else Stack.set q j (Node.get t' i')
      done;
      Stack.pop q
    | Node _, (Item (_, _, h') |  Bucket (_, h')) ->
      let i' = prefix land h' lsr d in
      begin
        match recunion q (d + shift) f (Node.get t i') t' with
        | Empty -> Node.remove t i'
        | t'' -> Node.set t i' t''
      end
    | Node _, Knot (b', _) ->
      Stack.push_node q t;
      let m' = ref b' in
      let s' = Knot.length t' in
      for i' = 1 to s' - 1 do
        let j = tzcnt !m' in m' := !m' land (!m' - 1);
        match recunion q (d + shift) f (Stack.get q j) Knot.(t'@i') with
        | Empty -> Stack.clear q j
        | t'' -> Stack.set q j t''
      done;
      Stack.pop q
    | Node _, Node _ ->
      Stack.push q;
      for i = 0 to size - 1 do
        match recunion q (d + shift) f (Node.get t i) (Node.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      Stack.pop q
    | Item _, (Bucket _ | Knot _ | Node _)
    | Bucket _, (Knot _ | Node _)
    | Knot _, Node _ ->
      recunion q d f t' t
  let union f t t' = recunion (Stack.make ()) 0 f t t'

  let rec recjoin q d f t t' = match t, t' with
    | _, _ when t == t' -> t
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Item (k, a, h), Item (k', a', h') ->
      if h = h' && Key.equal k k' then
        match f k a a' with
        | None -> Empty
        | Some a'' -> Item (k, a'', h)
      else Empty
    | Bucket (b, h), Item (k', a', h') ->
      if h = h' then
        try
          match f k' (Bucket.find k' b) a' with
          | None -> Empty
          | Some a'' -> Item (k', a'', h')
        with Not_found -> Empty
      else Empty
    | Bucket (b, h), Bucket (b', h') ->
      if h = h' then
        match Bucket.join f b b' with
        | [] -> Empty
        | b'' -> Bucket (b'', h)
      else Empty
    | Knot (b, _), (Item (_, _, h') | Bucket (_, h')) ->
      let i' = prefix land h' lsr d in
      if Knot.mem b i' then
        match recjoin q (d + shift) f (Knot.get t i') t' with
        | Empty -> Empty
        | (Item _ | Bucket _) as t'' -> t''
        | t'' -> Knot.make i' t''
      else Empty
    | Knot (b, _), Knot (b', _) ->
      Stack.push q;
      let m = ref (b land b') in
      while !m != 0 do
        let j = tzcnt !m in m := !m land (!m - 1);
        match recjoin q (d + shift) f (Knot.get t j) (Knot.get t' j) with
        | Empty -> ()
        | t'' -> Stack.set q j t''
      done;
      Stack.pop q
    | Node _, (Item (_, _, h') | Bucket (_, h')) ->
      let i' = prefix land h' lsr d in
      begin
        match recjoin q (d + shift) f (Node.get t i') t' with
        | Empty -> Empty
        | (Item _ | Bucket _) as t'' -> t''
        | t'' -> Knot.make i' t''
      end
    | Node _, Knot (b', _) ->
      Stack.push q;
      let m = ref b' in
      let s = Knot.length t' in
      for i = 1 to s - 1 do
        let j' = tzcnt !m in m := !m land (!m - 1);
        match recjoin q (d + shift) f (Node.get t j') Knot.(t'@i) with
        | Empty -> ()
        | t'' -> Stack.set q j' t''
      done;
      Stack.pop q
    | Node _, Node _ ->
      Stack.push q;
      for i = 0 to size - 1 do
        match recjoin q (d + shift) f (Node.get t i) (Node.get t' i) with
        | Empty -> ()
        | t'' -> Stack.set q i t''
      done;
      Stack.pop q
    | Item _, (Bucket _ | Knot _ | Node _)
    | Bucket _, (Knot _ | Node _)
    | Knot _, Node _ ->
      recjoin q d f t' t
  let join f t t' = recjoin (Stack.make ()) 0 f t t'
end
