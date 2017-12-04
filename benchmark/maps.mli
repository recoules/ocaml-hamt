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

module Listeq : S

module Hashtbl : S

module Map : S

module Rbmap : S

module Framac : S

module Bobatkey : S

module Ptfilliatr : S

module Thizanne : S

module HamtRef : S

module HamtLow : S

module HamtLazy : S
