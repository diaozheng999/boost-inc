(** Adapted from Self-Adjusting Computation by Umut Acar *)

type t = Types_internal.time
type window = t * t

val init: unit -> unit
val create: unit -> t
val add: t -> t
val getNext: t -> t option
val spliceOut: t -> t -> unit
val isSplicedOut: t -> bool 
val compare: t -> t -> Boost.Traits.ord
val compareWindow: window -> window -> Boost.Traits.ord

val describeTime: unit -> unit
val toString: t -> string
val inspectTime: unit -> string
