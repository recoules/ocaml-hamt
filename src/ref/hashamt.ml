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
    let union f b b' : 'a t = List.fold_left
        (fun u ((k, v) as a) ->
           if List.mem_assoc k b then u else a :: u)
        (List.fold_left
           (fun u ((k, v) as a) ->
              try
                match f k v (List.assoc k b') with
                | None -> u
                | Some v' -> (k, v') :: u
              with Not_found -> a :: u)
           [] b) b'
    let join f b b' : 'a t =
      List.fold_left
        (fun j (k, v) ->
           try
             match f k v (List.assoc k b') with
             | None -> j
             | Some v' -> (k, v') :: j
           with Not_found -> j)
        [] b
  end [@@inlined]

  type 'a t =
    | Empty
    | Item  of key * 'a * int
    | Bucket of 'a Bucket.t * int
    | Knot   of 'a t array * int
    | Node   of 'a t array * int

  module Knot = struct
    let select i = 1 lsl i [@@inline]
    let mask i = 1 lsl i - 1 [@@inline]

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

    let mem x b : bool = (b land select x) != 0 [@@inlined]
    let get x v b : 'a = v.(popcnt (b land mask x)) [@@inlined]
    let set x a v b : unit = v.(popcnt (b land mask x)) <- a [@@inlined]

    let single x a = Knot ([|a|], select x)

    let add x a v b =
      let c = popcnt (b land mask x) in
      let s = Array.length v + 1 in
      if s > 3 * size / 4 then
        let j = ref (-1) in
        let v = Array.init size
            (fun i ->
               if mem i b then
                 begin j := !j + 1; v.(!j) end
               else Empty) in
        v.(x) <- a;
        Node (v, s)
      else
        let v' = Array.make s Empty in
        Array.blit v 0 v' 0 c;
        v'.(c) <- a;
        Array.blit v c v' (c + 1) (Array.length v - c);
        Knot (v', b lor select x)

    let update x a v b =
      let c = popcnt (b land mask x) in
      let v = Array.copy v in v.(c) <- a;
      Knot (v, b)

    let remove x v b =
      let c = popcnt (b land mask x) in
      let v' = Array.make (Array.length v - 1) Empty in
      Array.blit v 0 v' 0 c;
      Array.blit v (c + 1) v' c (Array.length v - c - 1);
      Knot (v', b lxor select x)

    let compact v s =
      let b = ref 0 in let j = ref 0 in
      let v' = Array.init s
          (fun _ -> while v.(!j) = Empty do j := !j + 1 done;
            b := !b lor select !j; j := !j + 1; v.(!j - 1)) in
      Knot (v', !b)
  end

  module Node = struct
    let of_knot v b =
      let v' = Array.make size Empty in
      for i = 0 to size - 1 do
        if Knot.mem i b then v'.(i) <- Knot.get i v b
      done;
      v'
  end

  let empty = Empty

  let rec recadd d k a h t = match t with
    | Empty ->
      Item (k, a, h)
    | Item (k', a', h') ->
      if h = h' then
        if Key.equal k k' then
          Item (k, a, h)
        else
          Bucket (Bucket.add k' a' [(k, a)], h)
      else
        let i' = prefix land h' lsr d in
        recadd d k a h (Knot.single i' t)
    | Bucket (l, h') ->
      if h = h' then
        Bucket (Bucket.add k a l, h)
      else
        let i' = prefix land h' lsr d in
        recadd d k a h (Knot.single i' t)
    | Knot (v', b') ->
      let i = prefix land h lsr d in
      if Knot.mem i b' then
        Knot.update i (recadd (d + shift) k a h (Knot.get i v' b')) v' b'
      else Knot.add i (Item (k, a, h)) v' b'
    | Node (v', s') ->
      let i = prefix land h lsr d in
      let v'' = Array.copy v' in
      let s' = if v'.(i) = Empty then s' + 1 else s' in
      v''.(i) <- recadd (d + shift) k a h v'.(i);
      Node (v'', s')
  let add k a t = recadd 0 k a (Key.hash k) t

(*  let rec relink w t = match w with
    | [] -> t
    | (i, t') :: w' -> match t' with
      | Knot (v', b') ->
        relink w' (Knot.update i t v' b')
      | Node (v', s') ->
        let v'' = Array.copy v' in
        v''.(i) <- t;
        relink w' (Node (v'', s'))
      | _ -> assert false
  let rec fallink w t = match w, t with
    | (i, Knot (v', b')) :: w', (Item _ | Bucket _) ->
      if Array.length v' = 1 then
        fallink w' t
      else relink w t
    | _ -> relink w t
  let rec delink w = match w with
    | [] -> Empty
    | (i, t') :: w' -> match t' with
      | Knot (v', b') ->
        if Array.length v' = 1 then
          delink w'
        else if Array.length v' = 2 then begin
          let t = v'.(1 - (Knot.popcnt (b' land Knot.mask i))) in
          match t with
          | Item _ | Bucket _ -> fallink w' t
          | _ -> relink w' (Knot.remove i v' b')
        end else relink w' (Knot.remove i v' b')
      | Node (v', s') ->
        if s' = 3 * size / 4 then begin
          let t'' = v'.(i) in
          v'.(i) <- Empty;
          let t' = Knot.compact v' (s' - 1) in
          v'.(i) <- t'';
          relink w' t'
        end else begin
          let v'' = Array.copy v' in
          v''.(i) <- Empty;
          relink w' (Node (v'', s' - 1))
        end
      | _ -> assert false
  let rec recremove d k h t o w = match t with
    | Empty -> o
    | Item (k', a', h') ->
      if h = h' && Key.equal k k' then delink w else o
    | Bucket (l', h') ->
      if h = h' then
        match Bucket.remove k l' with
        | [] -> Empty
        | l'' when l' == l'' -> o
        | l'' -> relink w (Bucket (l'', h'))
      else o
    | Knot (v', b') ->
      let i = prefix land h lsr d in
      if Knot.mem i b' then
        recremove (d + shift) k h (Knot.get i v' b') o ((i, t) :: w)
      else o
    | Node (v', s') ->
      let i = prefix land h lsr d in
      recremove (d + shift) k h (v'.(i)) o ((i, t) :: w)
  let remove k t = recremove 0 k (Key.hash k) t t []*)

  let rec recremove d k h t = match t with
    | Empty -> Empty
    | Item (k', a', h') ->
      if h = h' && Key.equal k k' then Empty else t
    | Bucket (l', h') ->
      if h = h' then
        match Bucket.remove k l' with
        | [] -> Empty
        | l'' when l' == l'' -> t
        | l'' -> Bucket (l'', h')
      else t
    | Knot (v', b') ->
      let i = prefix land h lsr d in
      if Knot.mem i b' then
        let t' = Knot.get i v' b' in
        let t'' = recremove (d + shift) k h t' in
        match t'' with
        | t'' when t'' == t' -> t
        | Empty ->
          if Array.length v' = 1 then Empty
          else if Array.length v' = 2 then begin
            let i' = 1 - (Knot.popcnt (b' land Knot.mask i)) in
            let u = v'.(i') in
            match u with
            | Item _ | Bucket _ -> u
            | _ -> Knot.remove i v' b'
          end else Knot.remove i v' b'
        | Item _  | Bucket _ ->
          if Array.length v' = 1 then t''
          else Knot.update i t'' v' b'
        | t'' -> Knot.update i t'' v' b'
      else t
    | Node (v', s') ->
      let i = prefix land h lsr d in
      let t' = v'.(i) in
      match recremove (d + shift) k h t' with
      | t'' when t'' == t' -> t
      | Empty ->
        if s' <= 3 * size / 4 then begin
          v'.(i) <- Empty;
          let t'' = Knot.compact v' (s' - 1) in
          v'.(i) <- t';
          t''
        end else begin
          let v'' = Array.copy v' in v''.(i) <- Empty; Node (v'', s' - 1)
        end
      | t'' -> let v'' = Array.copy v' in v''.(i) <- t''; Node (v'', s')
  let remove k t = recremove 0 k (Key.hash k) t


  let rec recmem d k h t = match t with
    | Empty -> false
    | Item (k', _, h') -> if h = h' then Key.equal k k' else false
    | Bucket (l', h') -> if h = h' then Bucket.mem k l' else false
    | Knot (v', b') ->
      let i = prefix land h lsr d in
      if Knot.mem i b' then recmem (d + shift) k h (Knot.get i v' b')
      else false
    | Node (v', _) ->
      let i = prefix land h lsr d in
      recmem (d + shift) k h v'.(i)
  let mem k t = recmem 0 k (Key.hash k) t

  let rec recfind d k h t = match t with
    | Empty -> raise Not_found
    | Item (k', a', h') ->
      if h = h' && Key.equal k k' then a' else raise Not_found
    | Bucket (l', h') -> if h = h' then Bucket.find k l' else raise Not_found
    | Knot (v', b') ->
      let i = prefix land h lsr d in
      if Knot.mem i b' then recfind (d + shift) k h (Knot.get i v' b')
      else raise Not_found
    | Node (v', _) ->
      let i = prefix land h lsr d in
      recfind (d + shift) k h v'.(i)
  let find k t = recfind 0 k (Key.hash k) t

  let rec recbindings b l = match l with
    | [] -> b
    | t :: l ->
      match t with
      | Empty -> recbindings b l
      | Item (k', a', _) -> recbindings ((k', a') :: b) l
      | Bucket (l', _) ->
        recbindings (Bucket.fold (fun k v l' -> (k, v) :: l') l' b) l
     | Node (v', _) | Knot (v', _) ->
        recbindings b (Array.fold_left (fun l t -> t :: l) l v')
  and bindings m = recbindings [] [m]

  let rec recunion d f t t' = match t, t' with
    | Empty, _ -> t'
    | _, Empty -> t
    | _, _ when t == t' -> t
    | Item (k, a, h), Item (k', a', h') ->
      if h = h' && Key.equal k k' then
        match f k a a' with
        | None -> Empty
        | Some a'' -> Item (k, a'', h)
      else
        recadd d k a h t'
    | Bucket (b, h), Item (k', a', h') ->
      if h = h' && Bucket.mem k' b then
        recunion d f t (Bucket ([(k', a')], h'))
      else
        recadd d k' a' h' t
    | Bucket (b, h), Bucket (b', h') ->
      if h = h' then
        Bucket (Bucket.union f b b', h)
      else
        let i' = prefix land h' lsr d in
        recunion d f t (Knot.single i' t')
    | Knot (v, b), Item (_, _, h') | Knot (v, b), Bucket (_, h') ->
      let i' = prefix land h' lsr d in
      if Knot.mem i' b then
        match recunion (d + shift) f (Knot.get i' v b) t' with
        | Empty -> Knot.remove i' v b
        | t'' -> Knot.update i' t'' v b
      else Knot.add i' t' v b
    | Knot (v, b), Knot (v', b') ->
      let b'' = b lor b' in
      let s = Knot.popcnt b'' in
      if s > 3 * size / 4 then
         let v'' = Node.of_knot v b in
         for i = 0 to size - 1 do
           if Knot.mem i b' then
             match recunion (d + shift) f v''.(i) (Knot.get i v' b') with
             | Empty -> v''.(i) <- Empty
             | t'' -> v''.(i) <- t''
         done;
         Node (v'', s)
      else
        Knot.(
          let v'' = Array.make s Empty in
          for i = 0 to size - 1 do
            if mem i b && mem i b' then
              match recunion (d + shift) f (get i v b) (get i v' b') with
              | Empty -> ()
              | t'' -> set i t'' v'' b''
            else if mem i b then
              set i (get i v b) v'' b''
            else if mem i b' then
              set i (get i v' b') v'' b''
          done;
          Knot (v'', b''))
    | Node (v, s), Item (_, _, h') | Node (v, s), Bucket (_, h') ->
      let i' = prefix land h' lsr d in
      begin
        match recunion (d + shift) f v.(i') t' with
        | Empty ->
          let v' = Array.copy v in
          v'.(i') <- Empty;
          Node (v', s - 1)
        | t'' ->
          let v' = Array.copy v in
          v'.(i') <- t'';
          Node (v', s)
      end
    | Node (v, _), Knot (v', b') ->
      let v'' = Array.copy v in
      let s = ref 0 in
      for i = 0 to size - 1 do
        if Knot.mem i b' then
          match recunion (d + shift) f v''.(i) (Knot.get i v' b') with
          | Empty -> v''.(i) <- Empty
          | t'' -> v''.(i) <- t''; s := !s + 1
        else if v''.(i) != Empty then s := !s + 1
      done;
      Node (v'', !s)
    | Node (v, _), Node (v', _) ->
      let v'' = Array.copy v in
      let s = ref 0 in
      for i = 0 to size - 1 do
        match recunion (d + shift) f v''.(i) v'.(i) with
        | Empty -> v''.(i) <- Empty
        | t'' -> v''.(i) <- t''; s := !s + 1
      done;
      Node (v'', !s)
    | Item _, Bucket _
    | Item _, Knot _ | Bucket _, Knot _
    | Item _, Node _ | Bucket _, Node _
    | Knot _, Node _ ->
      recunion d f t' t
  let union f t t' = recunion 0 f t t'

  let rec recjoin d f t t' = match t, t' with
    | _, _ when t == t' -> t
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Item (k, a, h), Item (k', a', h') ->
      if h = h' && Key.equal k k' then
        match f k a a' with
        | None -> Empty
        | Some a'' -> Item (k, a'', h)
      else Empty
    | Bucket (b, h), Item (k', a', h') ->
      if h = h' && Bucket.mem k' b then
        recjoin d f t (Bucket ([(k', a')], h'))
      else Empty
    | Bucket (b, h), Bucket (b', h') ->
      if h = h' then
        Bucket (Bucket.join f b b', h)
      else Empty
    | Knot (v, b), Item (_, _, h') | Knot (v, b), Bucket (_, h') ->
      let i' = prefix land h' lsr d in
      if Knot.mem i' b then
        match recjoin (d + shift) f (Knot.get i' v b) t' with
        | Empty -> Empty
        | t'' -> Knot.single i' t''
      else Empty
    | Knot (v, b), Knot (v', b') ->
      let s = ref 0 in
      Knot.(
        let v'' = Array.make size Empty in
        for i = 0 to size - 1 do
          if mem i b && mem i b' then
            match recjoin (d + shift) f (get i v b) (get i v' b') with
            | Empty -> ()
            | t'' -> v''.(i) <- t''; s := !s + 1
        done;
        if !s > 3 * size / 4 then
          Node (v'', !s)
        else if !s > 0 then
          compact v'' !s
        else Empty
      )
    | Node (v, s), Item (_, _, h') | Node (v, s), Bucket (_, h') ->
      let i' = prefix land h' lsr d in
      begin
        match recjoin (d + shift) f v.(i') t' with
        | Empty -> Empty
        | t'' -> Knot.single i' t''
      end
    | Node (v, _), Knot (v', b') ->
      let v'' = Array.make size Empty in
      let s = ref 0 in
      for i = 0 to size - 1 do
        if Knot.mem i b' then
          match recjoin (d + shift) f v.(i) (Knot.get i v' b') with
          | Empty -> ()
          | t'' -> v''.(i) <- t''; s := !s + 1
      done;
      if !s > 3 * size / 4 then
        Node (v'', !s)
      else if !s >= 1 then
        Knot.compact v'' !s
      else Empty
    | Node (v, _), Node (v', _) ->
      let v'' = Array.make size Empty in
      let s = ref 0 in
      for i = 0 to size - 1 do
        match recjoin (d + shift) f v.(i) v'.(i) with
        | Empty -> ()
        | t'' -> v''.(i) <- t''; s := !s + 1
      done;
      if !s > 3 * size / 4 then
        Node (v'', !s)
      else if !s > 0 then
        Knot.compact v'' !s
      else
        Empty
    | Item _, Bucket _
    | Item _, Knot _ | Bucket _, Knot _
    | Item _, Node _ | Bucket _, Node _
    | Knot _, Node _ ->
      recjoin d f t' t
  let join f t t' = recjoin 0 f t t'
end
