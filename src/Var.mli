open Basis
open Combinators

type 'a var = {
  modref: 'a Box.t modref;
  change: 'a -> unit;
  create: 'a -> 'a Box.t;
  eq: 'a Box.t equality;
  change_eagerly: 'a -> unit;
  deref: unit -> 'a;
  subscribe: ('a -> unit) -> unit -> unit;
  subscribe_box: ('a Box.t -> unit) -> unit -> unit;
  subscribe_uncurried: (('a -> unit [@bs]) -> (unit -> unit [@bs]) [@bs]);
  subscribe_box_uncurried: (('a Box.t -> unit [@bs]) -> (unit -> unit [@bs]) [@bs]);
  change_to : ?exec_after:('a -> unit) -> ('a -> 'a) -> unit;
  changeEagerly: 'a -> unit [@deprecated];
  subscribeBox: ('a Box.t -> unit) -> unit -> unit  [@deprecated];
  subscribe1: (('a -> unit [@bs]) -> (unit -> unit [@bs]) [@bs]) [@deprecated];
  subscribeBox1: (('a Box.t -> unit [@bs]) -> (unit -> unit [@bs]) [@bs]) [@deprecated];
}

type 'a t = 'a var

val change : 'a var -> 'a -> unit

val change_eagerly : 'a var -> 'a -> unit

val of_modref : ?eq:'a Box.t equality -> ?label:string -> ('a -> 'a Box.t) -> 'a Box.t modref -> 'a var

val make_var : ?eq: 'a Box.t equality -> ('a -> 'a Box.t) -> 'a -> 'a var

val make : ?label:string -> 'a -> 'a var

val empty : ?label:string -> unit -> 'a var

val int : int -> int var

val opt : 'a option -> 'a option var

val str : string -> string var

val make_with_custom_hash_function : hash:('a -> string) -> 'a -> 'a var

val make_assuming_same_type : ?label:string -> 'a -> 'a var

val of_combinator : 'a Box.t cc -> 'a var

val observe : f:('a Box.t -> 'b) -> 'a var -> 'b

val log : ?l:string -> 'a var -> unit

val (>>>) : 'a var -> ('a -> 'b cc) -> 'b cc

val changeEagerly:  'a var -> 'a -> unit [@@deprecated]

val createVarFromModref : ?eq:'a Box.t equality -> ?label:string -> ('a -> 'a Box.t) -> 'a Box.t modref -> 'a var [@@deprecated]

val create :  ?label:string -> 'a -> 'a var [@@deprecated]

val createVar : ?eq: 'a Box.t equality -> ('a -> 'a Box.t) -> 'a -> 'a var [@@deprecated]

val withCustomHashFunction : hash:('a -> string) -> 'a -> 'a var [@@deprecated]

val createAssumingSameType :  ?label:string -> 'a -> 'a var [@@deprecated]

val ofCombinator : 'a Box.t cc -> 'a var [@@deprecated]
