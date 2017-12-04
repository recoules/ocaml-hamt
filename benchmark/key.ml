type t = int

let hash a : int = a
let equal a b : bool = a = b
let compare : t -> t -> int = Pervasives.compare

module Set = struct
  let depth = 30

  type key = t
  type t =
    | Empty
    | Leaf of key
    | Open of t * t
    | Lend of t * t
    | Rend of t * t
    | Full of t * t

  let empty : t = Empty

  let (@) k d : bool = 1 lsl d land k > 0 [@@inline]
  let (|=) k d : key = 1 lsl d lor k [@@inline]
  let (&=) k d : key = lnot (1 lsl d) land k [@@inline]

  let rec recexpand d k t : key * t = match t with
    | Empty ->
      k, Leaf k
    | Leaf k' ->
      if k = k' then
        match k'@d with
        | true ->
          let k = k &= d in
          if d = depth then k, Full (t, Leaf k) else k, Open (t, Leaf k)
        | false ->
          let k = k |= d in
          if d = depth then k, Full (Leaf k, t) else k, Open (Leaf k, t)
      else
        begin
          match k@d, k'@d with
          | true, true ->
            begin
              match recexpand (d + 1) k t with
              | k, (Full _ as t) -> k, Lend (t, Empty)
              | k, t -> k, Open (t, Empty)
            end
          | false, true ->
            if d = depth then k, Full (t, Leaf k) else k, Open (t, Leaf k)
          | true, false ->
            if d = depth then k, Full (Leaf k, t) else k, Open (Leaf k, t)
          | false, false ->
            begin
              match recexpand (d + 1) k t with
              | k, (Full _ as t') -> k, Rend (Empty, t')
              | k, t' -> k, Open (Empty, t')
            end
        end
    | Open (t, t') ->
      begin
        match k@d with
        | true ->
          begin
            match recexpand (d + 1) k t with
            | k, (Full _ as t) -> k, Lend (t, t')
            | k, t -> k, Open (t, t')
          end
        | false ->
          begin
            match recexpand (d + 1) k t' with
            | k, (Full _ as t') -> k, Rend (t, t')
            | k, t' -> k, Open (t, t')
          end
      end
    | Lend (t, Empty) ->
      let k = k &= d in
      if d = depth then
        k, Full (t, Leaf k)
      else k, Lend (t, Leaf k)
    | Lend (t, t') ->
      begin
        let k = k &= d in
        match recexpand (d + 1) k t' with
        | k, (Full _ as t') -> k, Full (t, t')
        | k, t' -> k, Lend (t, t')
      end
    | Rend (Empty, t') ->
      let k = k |= d in
      if d = depth then
        k, Full (Leaf k, t')
      else k, Rend (Leaf k, t')
    | Rend (t, t') ->
      begin
        let k = k |= d in
        match recexpand (d + 1) k t with
        | k, (Full _ as t) -> k, Full (t, t')
        | k, t -> k, Rend (t, t')
      end
    | Full _ -> raise Not_found
  let expand t : key * t = recexpand 0 (Random.bits ()) t

  let rec recreduce d k t : key * t = match t with
    | Empty -> raise Not_found
    | Leaf k' -> k', Empty
    | Open (t, Empty) ->
      begin
        match recreduce (d + 1) k t with
        | k, Empty -> k, Empty
        | k, t -> k, Open (t, Empty)
      end
    | Open (Empty, t') ->
      begin
        match recreduce (d + 1) k t' with
        | k, Empty -> k, Empty
        | k, t' -> k, Open (Empty, t')
      end
    | Lend (t, Empty) ->
      begin
        match recreduce (d + 1) k t with
        | k, Empty -> k, Empty
        | k, t -> k, Lend (t, Empty)
      end
    | Rend (Empty, t') ->
      begin
        match recreduce (d + 1) k t' with
        | k, Empty -> k, Empty
        | k, t' -> k, Rend (Empty, t')
      end
    | Open (t, t') | Lend (t, t') | Rend (t, t') ->
      begin
        match k@d with
        | true ->
          let k, t = recreduce (d + 1) k t in k, Open (t, t')
        | false ->
          let k, t' = recreduce (d + 1) k t' in k, Open (t, t')
      end
    | Full (t, t') ->
      match k@d with
      | true ->
        let k, t = recreduce (d + 1) k t in k, Rend (t, t')
      | false ->
        let k, t' = recreduce (d + 1) k t' in k, Lend (t, t')
  let reduce t : key * t = recreduce 0 (Random.bits ()) t

  let rec recchoose d k t : key = match t with
    | Empty -> raise Not_found
    | Leaf k' -> k'
    | Open (t, Empty) -> recchoose (d + 1) k t
    | Open (Empty, t') -> recchoose (d + 1) k t'
    | Lend (t, Empty) -> recchoose (d + 1) k t
    | Rend (Empty, t') -> recchoose (d + 1) k t'
    | Open (t, t') | Lend (t, t') | Rend (t, t') | Full (t, t') ->
      match k@d with
      | true  -> recchoose (d + 1) k t
      | false -> recchoose (d + 1) k t'
  let choose t : key = recchoose 0 (Random.bits ()) t

  let rec recmiss d k t : key = match t with
    | Empty -> k
    | Leaf k' -> if k'@d then k &= d else k |= d
    | Open (t, t') ->
      begin
        match k@d with
        | true  -> recmiss (d + 1) k t
        | false -> recmiss (d + 1) k t'
      end
    | Lend (_, t') -> recmiss (d + 1) (k &= d) t'
    | Rend (t, _) -> recmiss (d + 1) (k |= d) t
    | Full _ -> raise Not_found
  let miss t : key = recmiss 0 (Random.bits ()) t
end
