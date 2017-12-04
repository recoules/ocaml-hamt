module type S = sig
  type key = Key.t
  type 'a t

  val name: string
  val pure: bool

  val empty: unit -> 'a t

  val add: key -> 'a -> 'a t -> 'a t
  val remove: key -> 'a t -> 'a t

  val mem: key -> 'a t -> bool
  val find: key -> 'a t -> 'a

  val bindings: 'a t -> (key * 'a) list

  val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val join: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val force: 'a t -> unit
end

module Listeq : S = struct
  let name = "list"
  let pure = true

  type key = Key.t
  type 'a t = (key * 'a) list

  let empty () : 'a t = []
  let add k a t : 'a t = (k, a) :: t
  let remove key t : 'a t =
      List.filter (fun (k, _) -> not (Key.equal key k)) t
  let mem key t : bool = List.mem_assoc key t
  let find key t : 'a = List.assoc key t
  let bindings t : (key * 'a) list = t
  let fold f t b : 'b = List.fold_left (fun b (k, v) -> f k v b) b t
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

  let force t = ()
end

module Hashtbl : S = struct
  let name = "hashtbl"
  let pure = false
  include Hashtbl.Make (Key)

  let empty () : 'a t = create 0

  let bindings t =
    fold (fun k a b -> (k, a) :: b) t []

  let union f t t' =
    let t, t' = if length t < length t' then copy t', t else copy t, t' in
    iter
      (fun k a ->
         try
           match f k a (find t k) with
           | None -> remove t k
           | Some a' -> replace t k a'
         with Not_found -> add t k a)
      t';
    t
  let join f t t' =
    let t, t' = if length t < length t' then t, t' else t', t in
    let t'' = create (length t) in
    iter
      (fun k a ->
         try
           match f k a (find t' k) with
           | None -> ()
           | Some a'' -> add t'' k a''
         with Not_found -> ())
      t;
    t''

  let add k v t : 'a t = replace t k v; t
  let remove k t : 'a t = remove t k; t

  let mem k t : bool = mem t k
  let find k t : 'a = find t k

  let force t = ()
end

module Map : S = struct
  let name = "map"
  let pure = true
  include Map.Make (Key)
  let empty () = empty
  let join f = merge
      (fun k a a' -> match a, a' with
         | Some a, Some a' -> f k a a'
         | _ -> None)
  let force t = ()
end

module Rbmap : S = struct
  let name = "rbmap"
  let pure = true
  include Rbmap.Make (Key)
  let union f t t' =
    fold (fun k a u -> if mem k t then u else add k a u)
      t' (fold (fun k a u ->
          try
            match f k a (find k t') with
            | None -> u
            | Some a' -> add k a' u
          with Not_found -> add k a u)
          t empty)
  let join f t t' =
    fold (fun k a u ->
        try
          match f k a (find k t') with
          | None -> u
          | Some a' -> add k a' u
        with Not_found -> u)
      t empty
  let bindings t = assert false
  let empty () = empty
  let force t = ()
end

module Framac : S = struct
  let name = "framac"
  let pure = true
  include Framac.Mergemap.Make (Key)
  let empty () = empty
  let bindings t =
    fold (fun k a b -> (k, a) :: b) t []
  let union f t t' = union
      (fun k a a' -> match f k a a' with None -> assert false | Some a'' -> a'')
      t t'
  let join = interq
  let force t = ()
end

module Bobatkey : S = struct
  let name = "bobatkey"
  let pure = true
  type key = int
  include Bobatkey.IntMap.LittleEndian
  let mem k t = try find k t; true with Not_found -> false
  let remove k t = assert false
  let empty () = empty
  let bindings t = assert false
  let union f t t' = assert false
  let join f t t' = assert false
  let force t = ()
end

module Ptfilliatr : S = struct
  let name = "patricia_trie"
  let pure = true
  include Ptfilliatr.Ptmap
  let empty () = empty
  let union f = merge
      (fun k a a' -> match a, a' with
         | Some a, Some a' -> f k a a'
         | Some _, None    -> a
         | None  , Some _  -> a'
         | None  , None    -> assert false)
  let join f = merge
    (fun k a a' -> match a, a' with
      | Some a, Some a' -> f k a a'
      | _ -> None)
  let force t = ()
end

module Thizanne : S = struct
  let name = "thizanne"
  let pure = true
  include Thizanne.Hamt.Make (Thizanne.Hamt.StdConfig) (Key)
  let empty () = empty
  let union f t t' = union_f
      (fun k a a' -> match f k a a' with None -> assert false | Some a'' -> a'')
      t t'
  let join f = merge
      (fun k a a' -> match a, a' with
         | Some a, Some a' -> f k a a'
         | _ -> None)
  let force t = ()
end

module HamtRef : S = struct
  let name = "hamt-ref"
  let pure = true
  include Hamtref.Hashamt.Make (Key)
  let empty () = empty
  let force t = ()
end

module HamtLow : S = struct
  let name = "hamt-low"
  let pure = true
  include Hamtlow.Hashamt.Make (Key)
  let empty () = empty
  let force t = ()
end

module HamtLazy : S = struct
  let name = "hamt-lazy"
  let pure = false
  include Hamtlazy.Hashamt.Make (Key)
  let empty () = empty
  let join f t t' = assert false
  let union f t t' = assert false
  let force = unthunk
end
