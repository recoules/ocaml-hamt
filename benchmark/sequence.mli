type t

val generate: ?seed:int -> ?base:Key.Set.t -> F.f -> int -> Key.Set.t * t

val exec:
  add:(Key.t -> int -> 'a -> 'a) ->
  rem:(Key.t -> 'a -> 'a) ->
  mem:(Key.t -> 'a -> bool) ->
  force:('a -> unit) ->
  t -> 'a -> 'a

val pp: Format.formatter -> t -> unit
