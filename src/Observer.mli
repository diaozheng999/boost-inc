type 'a observer

type 'a t = 'a observer

exception UnsubIncReader

val unsub: 'a t -> unit

val inspect: 'a observer -> 'a observer Inspect.inspector

val make: ?label:string -> ?once:bool -> ('a -> unit) -> 'a observer

val makeInc: ('a -> unit) -> Time.window -> 'a observer

val read: ('a observer -> unit) -> 'a observer -> unit

val exec: ('a observer list -> unit) -> 'a observer -> 'a -> 'a observer list -> unit
