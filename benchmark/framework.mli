module type Runnable = sig
  type t

  val default: unit -> t

  val exec: ?force:bool -> Sequence.t -> t -> t

  val run:
    ?n:int -> name:string -> ?init:(unit -> t) -> Sequence.t -> unit

  val union:
    ?n:int -> name:string ->
    ?init1:(unit -> t) -> ?init2:(unit -> t) ->
    (Key.t -> int -> int -> int option) -> unit

  val join:
    ?n:int -> name:string ->
    ?init1:(unit -> t) -> ?init2:(unit -> t) ->
    (Key.t -> int -> int -> int option) -> unit
end

module type Iterable = sig
  type u
  type w

  val defaults: unit -> u * w

  val exec: ?force:bool -> Sequence.t -> u * w -> u * w

  val run:
    ?n:int -> name:string ->
    ?init:(unit -> u * w) -> Sequence.t -> unit

  val union:
    ?n:int -> name:string ->
    ?init1:(unit -> u * w) -> ?init2:(unit -> u * w) ->
    (Key.t -> int -> int -> int option) -> unit

  val join:
    ?n:int -> name:string ->
    ?init1:(unit -> u * w) -> ?init2:(unit -> u * w) ->
    (Key.t -> int -> int -> int option) -> unit
end

module Unit (M: Maps.S) : Runnable
module List (U: Runnable) (W: Iterable) : Iterable
module Tail (U: Runnable) : Iterable
module Cons (U: Iterable) (W: Iterable) : Iterable

module Cache : sig
  val clear: unit -> unit
end
