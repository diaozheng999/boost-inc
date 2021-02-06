type 'a entry = ('a * Time.window option) option ref

type 'a memotable

type 'a t = 'a memotable

val create: unit -> 'a memotable

val set: 'a memotable -> Box.index -> 'a entry -> unit

val find: 'a memotable -> Box.index -> Time.t -> 'a entry
