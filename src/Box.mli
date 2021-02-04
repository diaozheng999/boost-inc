type index
type 'a t

val init: unit -> unit
val create: ?label:string -> 'a -> 'a t
val eq: 'a t -> 'a t -> bool
val fromInt: int -> int t
val fromString: string -> string t
val fromOption: 'a t option -> 'a t option t
val valueOf: 'a t -> 'a
val indexOf: 'a t -> index
