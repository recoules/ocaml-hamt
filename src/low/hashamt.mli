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


module Make(Key : HashedType) : S with type key = Key.t
