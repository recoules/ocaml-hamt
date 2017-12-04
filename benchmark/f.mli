type command = Add | Rem | Mem | Force
type hint = Hit | Miss
type f = command -> hint -> int -> int

val pp: Format.formatter -> hint -> unit
val add: f
val mem: f
val rem: f
val mix: f
