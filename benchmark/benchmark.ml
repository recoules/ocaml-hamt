type measurement =
  {
    nanos: int;
    minor_allocated: int;
    major_allocated: int;
    promoted: int;
    compactions: int;
    major_collections: int;
    minor_collections: int;
  }


let measure ?(n=1) ~init f : measurement =
  let a = init () in

  (* pre-run measurements *)
  let gc1 = Gc.quick_stat () in
  let t1 = Core.Time.now () in

  for i = 1 to n do
    ignore(f a)
  done;

  (* post-run measurements *)
  let t2 = Core.Time.now () in
  let gc2 = Gc.quick_stat () in

  (* save measurements *)
  let nanos = truncate
      (Core.Time.Span.to_ns (Core.Time.diff t2 t1)) in
  let minor_allocated = truncate
      (gc2.Gc.minor_words -. gc1.Gc.minor_words) in
  let major_allocated = truncate
      (gc2.Gc.major_words -. gc1.Gc.major_words) in
  let promoted = truncate
      (gc2.Gc.promoted_words -. gc1.Gc.promoted_words) in
  let compactions =
    (gc2.Gc.compactions - gc1.Gc.compactions) in
  let major_collections =
    (gc2.Gc.major_collections - gc1.Gc.major_collections) in
  let minor_collections =
    (gc2.Gc.minor_collections - gc1.Gc.minor_collections) in
  {
    nanos; minor_allocated; major_allocated; promoted;
    compactions; major_collections; minor_collections;
  }

let pp ppf m = Format.fprintf ppf "%d; %d; %d; %d; %d; %d; %d"
    m.nanos m.minor_allocated m.major_allocated m.promoted m.compactions
    m.major_collections m.minor_collections

let run ~name ?(n=1) ~init f =
  Gc.compact ();
  Format.printf "%s; %d; %a@." name n pp (measure ~n ~init f);
