
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