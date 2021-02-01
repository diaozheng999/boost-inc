module type Element = sig
  type t
  val compare: t -> t -> CmpImpl.ord
  val isValid: t -> bool
end

module type Priority_queue = sig
  type elt
  type t
  exception Empty

  val empty: unit -> t
  val isEmpty: t -> bool
  val findMin: t -> elt option
  val insert: elt -> t -> t
  val deleteMin: t -> elt
end

module type Modifiable = sig

  type 'a modref
  type 'a t = 'a modref
  type changeable
  type time

  val init: unit -> unit

  val empty: unit -> 'a modref
  val create: 'a -> 'a modref
  val modref: ('a modref -> changeable) -> 'a modref
  val read: 'a modref -> ('a -> changeable) -> changeable
  val write: 'a 

end
