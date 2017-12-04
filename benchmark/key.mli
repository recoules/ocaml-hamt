type t = int

val hash: t -> int
val equal: t -> t -> bool
val compare: t -> t -> int

module Set : sig
  type key = t
  type t

  val empty : t

  val expand : t -> key * t
  val reduce : t -> key * t

  val choose : t -> key
  val miss   : t -> key
end
