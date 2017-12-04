type command = Add | Rem | Mem | Force
type hint = Hit | Miss
type f = command -> hint -> int -> int

let pp ppf h = match h with
  | Hit -> Format.fprintf ppf "hit"
  | Miss -> Format.fprintf ppf "miss"

let add c h _ = match c, h with Add, Miss -> 100 | _ -> 0
let mem c h _ = match c, h with Mem, Hit -> 50 | Mem, Miss -> 100 | _ -> 0
let rem c h _ = match c, h with Rem, Hit -> 100 | _ -> 0
let mix c h i = match c, h with
  | Add, Miss when i = 0 -> 100
  | Add, Miss -> 18
  | Add, Hit  -> 37
  | Mem, Hit  -> 50
  | Mem, Miss -> 68
  | Rem, Hit  -> 83
  | Rem, Miss -> 100
  | Force, _ -> 0
