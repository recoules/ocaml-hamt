OCAMLC=ocamlopt
LIBDIR=$(shell opam config var lib)

all: gen run

run: 	rbmap.cmx framac.cmx bobatkey.cmx ptfilliatr.cmx thizanne.cmx \
	hamtref.cmx lowhamt.o hamtlow.cmx hamtlazy.cmx \
	benchmark.mli benchmark.ml \
	f.mli f.ml key.mli key.ml \
	sequence.mli sequence.ml \
	maps.mli maps.ml \
	framework.mli framework.ml \
	run.ml
	ocamlfind $(OCAMLC) \
	  -I src \
	  -linkpkg -package core -thread \
	  -o $@ $^

gen:	f.mli f.ml key.mli key.ml \
	sequence.mli sequence.ml \
	gen.ml
	ocamlfind $(OCAMLC) \
	  -linkpkg -package core -thread \
	  -o $@ $^

rbmap.cmx: rbtree/rbmap.ml
	$(OCAMLC) -I rbtree -c -o $@ $^

framac.cmx: framac/hcons.cmx framac/listmap.cmx \
	    framac/intmap.cmx framac/mergemap.cmx
	$(OCAMLC) -I framac -pack -o $@ $^

framac/%.cmx: framac/%.mli framac/%.ml
	$(OCAMLC) -I framac -for-pack Framac -c $^


bobatkey.cmx: bobatkey/IntMap.cmx
	$(OCAMLC) -I bobatkey -pack -o $@ $^

bobatkey/%.cmx: bobatkey/%.ml
	$(OCAMLC) -I bobatkey -for-pack Bobatkey -c $^


ptfilliatr.cmx: ptfilliatr/ptmap.cmx
	$(OCAMLC) -I ptfilliatr -pack -o $@ $^

ptfilliatr/%.cmx: ptfilliatr/%.ml
	$(OCAMLC) -I ptfilliatr -for-pack Ptfilliatr -c $^



thizanne.cmx: thizanne/bitUtils.cmx thizanne/hamt.cmx
	$(OCAMLC) -I thizanne -pack -o $@ $^

thizanne/hamt.cmx: thizanne/hamt.mli thizanne/hamt.ml
	$(OCAMLC) -I thizanne -for-pack Thizanne -c $^

thizanne/bitUtils.cmx: thizanne/bitUtils.ml
	$(OCAMLC) -I thizanne -for-pack Thizanne -c $^



hamt%.cmx: ../src/%/hashamt.cmx
	$(OCAMLC) -pack -o $@ $^

../src/%/hashamt.cmx: \
	../src/%/hashamt.mli ../src/%/hashamt.ml
	$(OCAMLC) -I $(@D) \
		 -for-pack Hamt$* -c $^

lowhamt.o: ../src/low/lowhamt.c
	gcc -I $(LIBDIR)/ocaml/ \
            -mbmi -mbmi2 -mpopcnt \
	    -D CAML_INTERNALS -D CAML_NOASSERT -O3 -c $< -o $@



.PHONY: clean mrproper

clean:
	find .. -regex ".*\.\(o\|cmo\|cmx\|cmi\|s\)" -exec rm -rf {} \;

mrproper: clean
	rm -f run
	rm -f gen
