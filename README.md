# ocaml-hamt

This is the compilation of some experimentations on HAMT in OCaml.
The work was broadly inspired by the implementation of Thibault Suzanne and
it is separated in three versions:
 - a version of reference, in pure OCaml
 - a version with low level optimisation, mixing OCaml and C
 - a version with lazy behavior, in pure OCaml

As experimental, the code has been tested but not proved correct.

# Usage

The source files in src/ subfolders can be used independently.

The C code need to be compiled with the path to include ocaml header files and
at least the following flags: -mbmi -mbmi2 -mpopcnt -D CAML_INTERNALS

/!\ The low level version has been designed to work on amd64 processor only and
no fallback implementation is given yet.

# Benchmark

The versions have been tested against some others implementation of Map:
- OCaml Map
- OCaml Hashtbl
- Red Black Tree
- Patricia Tree (Filliatr)
- HAMT (Suzanne)

The benchmark/ folder contains a Makefile to generate two executables:
 - gen length path seed: generate three pseudo random (`seed) sequences of
`length operations (add, mem, rem) and store them in the `path folder
 - run length path seed repetition: measure the execution time and the memory
footprint of the corresponding sequences of operation generated by
gen `length `path `seed for all the benchmarked implementations

The data/ folder contains two scripts to respectively generate and mesure
the performance of the data structures over one thousand different seeds.
