let ( **) x y : int = (int_of_float ((float_of_int x)**(float_of_int y)))

open Framework

module Benchs = struct
  module Listeq = Unit (Maps.Listeq)
  module Hashtbl = Unit (Maps.Hashtbl)
  module Map = Unit (Maps.Map)
  module Rbmap = Unit (Maps.Rbmap)
  module Framac = Unit (Maps.Framac)
  module Bobatkey = Unit (Maps.Bobatkey)
  module Ptfilliatr = Unit (Maps.Ptfilliatr)
  module Thizanne = Unit (Maps.Thizanne)
  module HamtRef = Unit (Maps.HamtRef)
  module HamtLow = Unit (Maps.HamtLow)
end

open Benchs
module Standard = List (Hashtbl) (Tail (Map))
module Extra = Tail (Rbmap)
module Mergemap = List (Framac) (Tail (Ptfilliatr))
module Hamt = List (Thizanne) (List (HamtRef) (Tail (HamtLow)))

module All : Iterable =
  Cons (Cons (Standard) (Mergemap)) (Cons (Extra) (Hamt))

let argc = Array.length Sys.argv - 1
let () = if argc < 3 || argc > 4 then raise
      (Invalid_argument
         "usage: @ size seed prefix [repetitions]")

let i = int_of_string Sys.argv.(1)
let prefix = Sys.argv.(2)
let seed = int_of_string Sys.argv.(3)
let n = if argc = 4 then int_of_string Sys.argv.(4) else 1
let () =
  let nadd = Format.sprintf "%sadd.n%d.s%d.seq" prefix seed i in
  let fadd = open_in nadd in
  let core = Marshal.from_channel fadd in
  let nmem = Format.sprintf "%smem.n%d.s%d.seq" prefix seed i in
  let fmem = open_in nmem in
  let mem = Marshal.from_channel fmem in
  let nrem = Format.sprintf "%srem.n%d.s%d.seq" prefix seed i in
  let frem = open_in nrem in
  let rem = Marshal.from_channel frem in
  All.run ~n ~name:(Format.asprintf "%x; build; 1e%d; " seed i)
    core;
  let init () = All.exec core (All.defaults ()) in
  ignore(init ());
  All.run ~n ~name:(Format.asprintf "%x; mem; 1e%d; " seed i) ~init
    mem;
  All.run ~n ~name:(Format.asprintf "%x; rem; 1e%d; " seed i) ~init
    rem;
  All.join ~n
    ~name:(Format.asprintf "%x; join_equal; 1e%d; " seed i)
    ~init1:init ~init2:init
    (fun _ _ _ -> Some 0);
