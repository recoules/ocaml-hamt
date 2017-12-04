let popcnt i =
  let i = (i land 0x5555555555555555)
          + ((i lsr 1) land 0x5555555555555555) in
  let i = (i land 0x3333333333333333)
          + ((i lsr 2) land 0x3333333333333333) in
  let i = (i land 0x0f0f0f0f0f0f0f0f)
          + ((i lsr 4) land 0x0f0f0f0f0f0f0f0f) in
  (i * 0x0101010101010101) lsr (64 - 8) [@@inlined]

let tzcnt i =
  let rec tzcnt i j =
    if i land 1 = 0 then tzcnt (i lsr 1) (j + 1) else j in
  tzcnt i 0

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

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val join : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val unthunk : 'a t -> unit
end

module Make(Key : HashedType) : S with type key = Key.t = struct
  let size = 32
  let shift = 5
  let prefix = 0b11111

  type key = Key.t

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
  end

  type 'a t =
    | Empty
    | Root  of 'a tree ref
  and 'a tree =
    | Item  of key * 'a * int
    | Bucket of 'a Bucket.t * int
    | Knot   of 'a t array * int
    | Node   of 'a t array * int
    | Thunk  of 'a tree * 'a tree * int

  let select i = 1 lsl i [@@inline]
  let mask i = 1 lsl i - 1 [@@inline]

  module Knot = struct
    let mem x b : bool = (b land select x) != 0 [@@inlined]
    let lookup x b : int = popcnt (b land mask x) [@@inlined]
  end

  let empty = Empty

  let rec rev t l = match t with
    | Thunk (e, t', _) ->
      rev t' (e :: l)
    | _ -> t :: l

  let rec dispatchk d v b l =
    match l with
    | [] -> ()
    | e :: l' -> match e with
      | Item (k, a, h) ->
        begin
          let i = prefix land h lsr d in
          let j = Knot.lookup i b in
          match v.(j) with
          | Empty -> v.(j) <- Root (ref e)
          | Root tree -> match !tree with
            | Item (k', a', h') ->
              if h = h' then
                if Key.equal k k' then v.(j) <- Root (ref e)
                else
                  v.(j) <- Root (ref (Bucket ([(k, a); (k', a')], h)))
              else
                let i = prefix land h lsr (d + shift) in
                let i' = prefix land h' lsr (d + shift) in
                v.(j) <- Root (ref (Thunk (e, !tree, select i lor select i')))
            | Bucket (l', h') ->
              if h = h' then
                v.(j) <- Root (ref (Bucket ((k, a) :: l', h)))
              else
                let i = prefix land h lsr (d + shift) in
                let i' = prefix land h' lsr (d + shift) in
                v.(j) <- Root (ref (Thunk (e, !tree, select i lor select i')))
            | Knot (_, b') | Node (_, b') | Thunk (_, _, b') ->
              let i = prefix land h lsr (d + shift) in
              v.(j) <- Root (ref (Thunk (e, !tree, select i lor b')))
        end;
        dispatchk d v b l'
      | Bucket (_, h) ->
        let i = prefix land h lsr d in
        let j = Knot.lookup i b in
        v.(j) <- Root (ref e)
      | Knot (v', b') ->
        for i = 0 to size - 1 do
          let j = Knot.lookup i b in
          if Knot.mem i b' then
            let j' = Knot.lookup i b' in
            v.(j) <- v'.(j')
        done;
        dispatchk d v b l'
      | Node _ | Thunk _ -> assert false

  let rec dispatch d v l =
    match l with
    | [] -> ()
    | e :: l' -> match e with
      | Item (k, a, h) ->
        begin
          let j = prefix land h lsr d in
          match v.(j) with
          | Empty -> v.(j) <- Root (ref e)
          | Root tree -> match !tree with
            | Item (k', a', h') ->
              if h = h' then
                if Key.equal k k' then v.(j) <- Root (ref e)
                else v.(j) <- Root (ref (Bucket ([(k, a); (k', a')], h)))
              else
                let i = prefix land h lsr (d + shift) in
                let i' = prefix land h' lsr (d + shift) in
                v.(j) <- Root (ref (Thunk (e, !tree, select i lor select i')))
            | Bucket (l', h') ->
              if h = h' then v.(j) <- Root (ref (Bucket ((k, a) :: l', h)))
              else
                let i = prefix land h lsr (d + shift) in
                let i' = prefix land h' lsr (d + shift) in
                v.(j) <- Root (ref (Thunk (e, !tree, select i lor select i')))
            | Knot (_, b') | Node (_, b') | Thunk (_, _, b') ->
              let i = prefix land h lsr (d + shift) in
              v.(j) <- Root (ref (Thunk (e, !tree, select i lor b')))
        end;
        dispatch d v l'
      | Bucket (_, h) ->
        let j = prefix land h lsr d in
        v.(j) <- Root (ref e);
        dispatch d v l'
      | Knot (v', b') ->
        for j = 0 to size - 1 do
          let j' = Knot.lookup j b' in
          if Knot.mem j b' then v.(j) <- v'.(j')
        done;
        dispatch d v l'
      | Node (v', _) ->
        for j = 0 to size - 1 do
          v.(j) <- v'.(j)
        done;
        dispatch d v l'
      | Thunk _ -> assert false

  let unthunk d t = match t with
    | Thunk (k, t', b) ->
      begin
        let s = popcnt b in
        if s > 3 * size / 4 then
          let v = Array.make size Empty in
          dispatch d v (rev t []);
          Node (v, b)
        else
          let v = Array.make s Empty in
          dispatchk d v b (rev t []);
          Knot (v, b)
      end
    | _ -> assert false

  let add k a t = let h = Key.hash k in match t with
    | Empty -> Root (ref (Item (k, a, h)))
    | Root tree -> match !tree with
      | Item (k', a', h') ->
        if h = h' then
          if Key.equal k k' then Root (ref (Item (k, a, h)))
          else Root (ref (Bucket ([(k, a); (k', a')], h)))
        else
          let i = prefix land h in let i' = prefix land h' in
          Root (ref (Thunk (Item (k, a, h), !tree, select i lor select i')))
      | Bucket (l', h') ->
        if h = h' then Root (ref (Bucket ((k, a) :: l', h)))
        else
          let i = prefix land h in let i' = prefix land h' in
          Root (ref (Thunk (Item (k, a, h), !tree, select i lor select i')))
      | Knot (_, b') | Node (_, b') | Thunk (_, _, b') ->
        let i = prefix land h in
        Root (ref (Thunk (Item (k, a, h), !tree, select i lor b')))

  let remove k t = assert false

  let rec recmem d k h t = match t with
    | Empty -> false
    | Root tree -> match !tree with
      | Item (k', _, h') -> h = h' && Key.equal k k'
      | Bucket (l', h') -> h = h' && Bucket.mem k l'
      | Knot (v', b') ->
        let i = prefix land h lsr d in
        let j' = Knot.lookup i b' in
        Knot.mem i b' && recmem (d + shift) k h v'.(j')
      | Node (v', _) ->
        let i = prefix land h lsr d in
        recmem (d + shift) k h v'.(i)
      | Thunk _ -> tree := unthunk d !tree; recmem d k h t
  let mem k t = recmem 0 k (Key.hash k) t

  let rec recfind d k h t = match t with
    | Empty -> raise Not_found
    | Root tree -> match !tree with
      | Item (k', a', h') -> if h = h' then a' else raise Not_found
      | Bucket (l', h') -> if h = h' then Bucket.find k l' else raise Not_found
      | Knot (v', b') ->
        let i = prefix land h lsr d in
        let j' = Knot.lookup i b' in
        if Knot.mem i b' then recfind (d + shift) k h v'.(j')
        else raise Not_found
      | Node (v', _) ->
        let i = prefix land h lsr d in
        recfind (d + shift) k h v'.(i)
      | Thunk _ -> tree := unthunk d !tree; recfind d k h t
  let find k t = recfind 0 k (Key.hash k) t

  let rec recbindings b l = match l with
    | [] -> b
    | (d, t) :: l ->
      match t with
      | Empty -> recbindings b l
      | Root tree -> match !tree with
        | Item (k', a', _) -> recbindings ((k', a') :: b) l
        | Bucket (l', _) ->
          recbindings (Bucket.fold (fun k v l' -> (k, v) :: l') l' b) l
        | Node (v', _) | Knot (v', _) ->
          recbindings b (Array.fold_left (fun l t -> (d, t) :: l) l v')
        | Thunk _ -> tree := unthunk d !tree; recbindings b l
  and bindings m = recbindings [] [(0, m)]

  let union f t t' = assert false

  let join f t t' = assert false

  let rec recunthunk d t = match t with
    | Empty -> ()
    | Root tree -> match !tree with
      | Item _ | Bucket _ -> ()
      | Knot (v, _) | Node (v, _) ->
        for i = 0 to Array.length v - 1 do
          recunthunk (d + shift) v.(i)
        done
      | Thunk _ -> tree := unthunk d !tree; recunthunk d t
  let unthunk t = recunthunk 0 t
end
