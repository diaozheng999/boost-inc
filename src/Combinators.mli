(** Adapted from Self-Adjusting Computation by Umut Acar *)

open Basis

type 'a modref = 'a Modifiable.t
type 'a cc

type ('b, 'd) pad

val modref: 'a cc -> 'a modref
val create: 'a -> 'a modref
val write: 'a Box.t -> 'a Box.t cc
val write': 'a equality -> 'a -> 'a cc
val read: 'b modref -> ('b -> 'a cc) -> 'a cc
val memoize: 'a Memo_table.t -> Box.index -> (unit -> 'a) -> 'a
val mkLift: 'b equality -> Box.index -> 'b -> ('b modref -> 'd) -> 'd
val mkLiftCC: ?fname:string -> 'b equality -> 'd equality -> Box.index -> 'b -> ('b modref -> 'd cc) -> 'd cc

(** shorthand for `Combinators.read` *)
val (>>=): 'a modref -> ('a -> 'b cc) -> 'b cc

val log: 'a modref -> unit
