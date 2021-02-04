type index = int
type 'a t

val init: unit -> unit
val create: 'a -> 'a t
val eq: 'a t -> 'a t -> bool
val fromInt: int -> int t
val fromOption: 'a t option -> 'a t option t
val valueOf: 'a t -> 'a
val indexOf: 'a t -> index
