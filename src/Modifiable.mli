type 'a modref
type 'a t = 'a modref
type changeable
type time = Time.t

val init: unit -> unit

val empty: unit -> 'a modref
val create: 'a -> 'a modref
val modref: ('a modref -> changeable) -> 'a modref
val read: 'a modref -> ('a -> changeable) -> changeable
val write: 'a Box.t modref -> 'a Box.t -> changeable
val write': 'a CmpImpl.eq -> 'a modref -> 'a -> changeable

val change: 'a Box.t modref -> 'a Box.t -> unit
val change': 'a CmpImpl.eq -> 'a modref -> 'a -> unit
val change'': 'a modref -> 'a -> unit

val deref: 'a modref -> 'a
val propagate: unit -> unit Js.Promise.t
val propagateUntil: time -> unit Js.Promise.t

val latest: time ref
val finger: time ref

val isOutOfFrame: time -> time -> bool