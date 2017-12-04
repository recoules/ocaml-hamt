type t =
  | Empty
  | Add of F.hint * int * t
  | Rem of F.hint * int * t
  | Mem of F.hint * int * t
  | Force of t

let rec build f i size e l : Key.Set.t * t =
  if i < size then
    let r = Random.bits () mod 100 in
    if f F.Add F.Miss i > r then
      let k, e = Key.Set.expand e in
      build f (i + 1) size e (Add (F.Miss, k, l))
    else if f F.Add F.Hit i > r then
      build f (i + 1) size e (Add (F.Hit, Key.Set.choose e, l))
    else if f F.Rem F.Hit i > r then
      let k, e = Key.Set.reduce e in
      build f (i + 1) size e (Rem (F.Hit, k, l))
    else if f F.Rem F.Miss i > r then
      build f (i + 1) size e (Rem (F.Miss, Key.Set.miss e, l))
    else if f F.Mem F.Hit i > r then
      build f (i + 1) size e (Mem (F.Hit, Key.Set.choose e, l))
    else if f F.Mem F.Miss i > r then
      build f (i + 1) size e (Mem (F.Miss, Key.Set.miss e, l))
    else e, Force l
  else e, l

let rev t : t =
  let rec rev t r = match t with
    | Empty -> r
    | Add (h, k, t) -> rev t (Add (h, k, r))
    | Rem (h, k, t) -> rev t (Rem (h, k, r))
    | Mem (h, k, t) -> rev t (Mem (h, k, r))
    | Force t -> rev t (Force r) in
  rev t Empty

let generate ?(seed=42) ?(base=Key.Set.empty) f size : Key.Set.t * t =
  Random.init seed; let k, t = build f 0 size base Empty in k, rev t

let rec exec ~add ~rem ~mem ~force t m : 'a = match t with
  | Empty -> m
  | Add (_, i, t) -> exec ~add ~rem ~mem ~force t (add i i m)
  | Rem (_, i, t) -> exec ~add ~rem ~mem ~force t (rem i m)
  | Mem (F.Hit, i, t) ->
    assert (mem i m = true); exec ~add ~rem ~mem ~force t m
  | Mem (F.Miss, i, t) ->
    assert (mem i m = false); exec ~add ~rem ~mem ~force t m
  | Force t -> force m; exec ~add ~rem ~mem ~force t m

let rec pp ppf t : unit = match t with
  | Empty -> ()
  | Add (h, i, t) -> Format.fprintf ppf "add %a %x;" F.pp h i; pp ppf t
  | Rem (h, i, t) -> Format.fprintf ppf "rem %a %x;" F.pp h i; pp ppf t
  | Mem (h, i, t) -> Format.fprintf ppf "mem %a %x;" F.pp h i; pp ppf t
  | Force t -> Format.fprintf ppf "force; "; pp ppf t
