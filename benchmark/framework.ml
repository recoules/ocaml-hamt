module Cache = struct
  let clears = ref []

  let memoize1 f =
    let cache = Hashtbl.create 10 in
    clears := (fun () -> Hashtbl.clear cache) :: !clears;
    begin fun ?(force=false) arg0 ->
      if force then
        let res = f arg0 in Hashtbl.replace cache arg0 res; res
      else
        try Hashtbl.find cache arg0
        with Not_found -> let res = f arg0 in Hashtbl.add cache arg0 res; res
    end

  let memoize2 f =
    let cache = Hashtbl.create 10 in
    clears := (fun () -> Hashtbl.clear cache) :: !clears;
    begin fun ?(force=false) arg0 arg1 ->
      if force then
        let res = f arg0 arg1 in Hashtbl.replace cache (arg0, arg1) res; res
      else
        try Hashtbl.find cache (arg0, arg1)
        with Not_found ->
          let res = f arg0 arg1 in Hashtbl.add cache (arg0, arg1) res; res
    end

  let clear () = List.iter (fun c -> c ()) !clears
end

module type Runnable = sig
  type t

  val default: unit -> t

  val exec: ?force:bool -> Sequence.t -> t -> t

  val run:
    ?n:int -> name:string -> ?init:(unit -> t) -> Sequence.t -> unit

  val union:
    ?n:int -> name:string ->
    ?init1:(unit -> t) -> ?init2:(unit -> t) ->
    (Key.t -> int -> int -> int option) -> unit

  val join:
    ?n:int -> name:string ->
    ?init1:(unit -> t) -> ?init2:(unit -> t) ->
    (Key.t -> int -> int -> int option) -> unit
end

module Unit (M: Maps.S) : Runnable = struct
  type t = int M.t

  let default = M.empty

  let exec =
    if M.pure then Cache.memoize2
        (Sequence.exec ~add:M.add ~rem:M.remove ~mem:M.mem ~force:M.force)
    else fun ?force ->
      Sequence.exec ~add:M.add ~rem:M.remove ~mem:M.mem ~force:M.force

  let run ?(n=1) ~name ?(init=default) cmd : unit =
    Benchmark.run ~name:(Format.sprintf "%s%s" name M.name)
      ~n ~init (exec ~force:true cmd)

  let safe = false

  module Set = struct
    include Set.Make (Key)
    let of_list l = List.fold_left (fun t (k, _) -> add k t) empty l
    let join t t' =
      fold (fun k t'' -> if mem k t' then add k t'' else t'') t empty
  end

  let union ?(n=1) ~name ?(init1=default) ?(init2=default) f : unit =
    let init () = init1 (), init2 () in
    Benchmark.run ~name:(Format.sprintf "%s%s" name M.name)
      ~n ~init (fun (t, t') ->
          if safe then
            let s = Set.of_list (M.bindings t) in
            let s' = Set.of_list (M.bindings t') in
            let s'' = Set.union s s' in
            let s''' = Set.of_list (M.bindings (M.union f t t')) in
            assert (Set.equal s'' s''')
          else ignore(M.union f t t'))

  let join ?(n=1) ~name ?(init1=default) ?(init2=default) f : unit =
    let init () = init1 (), init2 () in
    Benchmark.run ~name:(Format.sprintf "%s%s" name M.name)
      ~n ~init (fun (t, t') ->
          if safe then
            let s = Set.of_list (M.bindings t) in
            let s' = Set.of_list (M.bindings t') in
            let s'' = Set.join s s' in
            let s''' = Set.of_list (M.bindings (M.join f t t')) in
            assert (Set.equal s'' s''')
          else ignore(M.join f t t'))
end

module type Iterable = sig
  type u
  type w

  val defaults: unit -> u * w

  val exec: ?force:bool -> Sequence.t -> u * w -> u * w

  val run:
    ?n:int -> name:string ->
    ?init:(unit -> u * w) -> Sequence.t -> unit

  val union:
    ?n:int -> name:string ->
    ?init1:(unit -> u * w) -> ?init2:(unit -> u * w) ->
    (Key.t -> int -> int -> int option) -> unit

  val join:
    ?n:int -> name:string ->
    ?init1:(unit -> u * w) -> ?init2:(unit -> u * w) ->
    (Key.t -> int -> int -> int option) -> unit
end

module List (U: Runnable) (W: Iterable) : Iterable = struct
  type u = unit -> U.t
  type w = unit -> W.u * W.w

  let defaults () = U.default, W.defaults

  let exec ?(force=false) cmd (u, w) =
    (fun () -> U.exec ~force cmd (u ())),
    (fun () -> W.exec ~force cmd (w ()))

  let run ?(n=1) ~name ?(init=defaults) cmd =
    let u, w = init () in
    U.run ~n ~name ~init:u cmd;
    W.run ~n ~name ~init:w cmd

  let join ?(n=1) ~name ?(init1=defaults) ?(init2=defaults) f =
    let u1, w1 = init1 () in let u2, w2 = init2 () in
    U.join ~n ~name ~init1:u1 ~init2:u2 f;
    W.join ~n ~name ~init1:w1 ~init2:w2 f

  let union ?(n=1) ~name ?(init1=defaults) ?(init2=defaults) f =
    let u1, w1 = init1 () in let u2, w2 = init2 () in
    U.union ~n ~name ~init1:u1 ~init2:u2 f;
    W.union ~n ~name ~init1:w1 ~init2:w2 f
end

module Tail (U: Runnable) : Iterable = struct
  type u = unit -> U.t
  type w = unit

  let defaults () = U.default, ()

  let exec ?(force=false) cmd (u, ()) =
    (fun () -> U.exec ~force cmd (u ())), ()

  let run ?(n=1) ~name ?(init=defaults) cmd =
    let u, _ = init () in U.run ~n ~name ~init:u cmd

  let join ?(n=1) ~name ?(init1=defaults) ?(init2=defaults) f =
    let u1, _ = init1 () in let u2, _ = init2 () in
    U.join ~n ~name ~init1:u1 ~init2:u2 f

  let union ?(n=1) ~name ?(init1=defaults) ?(init2=defaults) f =
    let u1, _ = init1 () in let u2, _ = init2 () in
    U.union ~n ~name ~init1:u1 ~init2:u2 f
end

module Cons (U: Iterable) (W: Iterable) : Iterable = struct
  type u = unit -> U.u * U.w
  type w = unit -> W.u * W.w

  let defaults () = U.defaults, W.defaults

  let exec ?(force=false) cmd (u, w) =
    (fun () -> U.exec ~force cmd (u ())),
    (fun () -> W.exec ~force cmd (w ()))

  let run ?(n=1) ~name ?(init=defaults) cmd =
    let u, w = init () in
    U.run ~n ~name ~init:u cmd;
    W.run ~n ~name ~init:w cmd

  let join ?(n=1) ~name ?(init1=defaults) ?(init2=defaults) f =
    let u1, w1 = init1 () in let u2, w2 = init2 () in
    U.join ~n ~name ~init1:u1 ~init2:u2 f;
    W.join ~n ~name ~init1:w1 ~init2:w2 f

  let union ?(n=1) ~name ?(init1=defaults) ?(init2=defaults) f =
    let u1, w1 = init1 () in let u2, w2 = init2 () in
    U.union ~n ~name ~init1:u1 ~init2:u2 f;
    W.union ~n ~name ~init1:w1 ~init2:w2 f
end
