open Basis

type 'a modref
type 'a t = 'a modref
type changeable
type time = Time.t

val init: unit -> unit

val advanceTime: unit -> unit

val empty: unit -> 'a modref
val create: 'a -> 'a modref
val modref: ('a modref -> changeable) -> 'a modref
val read: 'a modref -> ('a -> changeable) -> changeable
val write: 'a Box.t modref -> 'a Box.t -> changeable
val write': 'a equality -> 'a modref -> 'a -> changeable

val change: 'a Box.t modref -> 'a Box.t -> unit
val change': 'a equality -> 'a modref -> 'a -> unit
val change'': 'a modref -> 'a -> unit

val deref: 'a modref -> 'a
val deref': 'a modref -> 'a option
val propagate: unit -> unit
val propagateUntil: time -> unit

val latest: time ref
val finger: time ref

val isOutOfFrame: time -> time -> bool

val observe: 'a modref -> ('a -> 'b) -> 'b

val attachObserver: 'a modref -> ?label:string -> ('a -> unit) -> 'a Observer.t
val attachObserver1: 'a modref -> ?label:string -> ('a -> unit) Js.Fn.arity1 -> 'a Observer.t

val attach_observer_once : 'a modref -> ?label:string -> ('a -> unit) -> 'a Observer.t
