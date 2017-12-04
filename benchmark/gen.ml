let ( **) x y : int = (int_of_float ((float_of_int x)**(float_of_int y)))

let argc = Array.length Sys.argv - 1

let i = int_of_string Sys.argv.(1)
let prefix = Sys.argv.(2)
let seed = if argc > 2 then int_of_string Sys.argv.(3) else 42


let () =
  let base, core = Sequence.generate ~seed F.add (10**i) in
  let nadd = Format.sprintf "%sadd.n%d.s%d.seq" prefix seed i in
  let fadd = open_out nadd in
  Marshal.to_channel fadd core [Marshal.No_sharing];
  close_out fadd;
  let _, mem = Sequence.generate ~seed ~base F.mem (10**i) in
  let nmem = Format.sprintf "%smem.n%d.s%d.seq" prefix seed i in
  let fmem = open_out nmem in
  Marshal.to_channel fmem mem [Marshal.No_sharing];
  close_out fmem;
  let _, rem = Sequence.generate ~seed ~base F.rem (10**i) in
  let nrem = Format.sprintf "%srem.n%d.s%d.seq" prefix seed i in
  let frem = open_out nrem in
  Marshal.to_channel frem rem [Marshal.No_sharing];
  close_out frem
