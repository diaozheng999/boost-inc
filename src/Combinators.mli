open CmpImpl

type 'a modref = 'a Modifiable.t
type 'a cc

val modref: 'a cc -> 'a modref
val create: 'a -> 'a modref
val write: 'a Box.t -> 'a Box.t cc
val write': 'a eq -> 'a -> 'a cc
val read: 'b modref -> ('b -> 'a cc) -> 'a cc
val memoize: 'a Memo_table.t -> int list -> (unit -> 'a) -> 'a
val mkLift: 'b eq -> int list -> 'b -> ('b modref -> 'd) -> 'd
val mkLift2: 'b eq -> 'c eq -> int list -> 'b -> 'c -> ('b modref -> 'c modref -> 'd) -> 'd
val mkLiftCC: 'b eq -> 'd eq -> int list -> 'b -> ('b modref -> 'd cc) -> 'd cc
val mkLiftCC2:
  'b eq -> 'c eq -> 'd eq 
    -> int list -> 'b -> 'c
    -> ('b modref -> 'c modref -> 'd cc) -> 'd cc

val (>>=): 'a modref -> ('a -> 'b cc) -> 'b cc

val log: 'a modref -> unit
