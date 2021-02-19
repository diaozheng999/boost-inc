type t
type window = t * t

val init: unit -> unit
val create: unit -> t
val add: t -> t
val getNext: t -> t option
val spliceOut: t -> t -> unit
val isSplicedOut: t -> bool 
val compare: t -> t -> CmpImpl.ord
val compareWindow: window -> window -> CmpImpl.ord

val describeTime: unit -> unit
val toString: t -> string
val inspectTime: unit -> string
