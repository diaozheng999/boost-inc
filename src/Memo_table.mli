type 'a memotable

type 'a t = 'a memotable

type 'a entry = ('a * Time.window option) option ref

val create: unit -> 'a memotable

val find: 'a memotable -> int list -> Time.t -> 'a entry
