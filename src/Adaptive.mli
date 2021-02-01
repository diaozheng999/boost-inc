type 'a modifiable
type 'a dest

type changeable
(*
val modifiable: ('a -> 'a -> bool) -> ('a dest -> changeable) -> 'a modifiable

val read: 'a modifiable -> ('a -> changeable) -> changeable
val write: 'a dest -> 'a -> changeable

val init: unit -> unit
val change: 'a modifiable -> 'a -> unit
val propagate: unit -> unit
*)