type t

type 'a cc

val variable : 'a cc -> 'a Variable.t

val create : 'a -> 'a Variable.t

val write : 'a -> 'a cc

val read : 'a Variable.t -> ('a -> 'b cc) -> 'b cc

val mk_lift :
  name:string ->
  'a Variable.t ->
  ('a Variable.t -> 'b Async_callback.t) ->
  'b Js.Promise.t

val mk_lift_cc :
  name:string -> 'a Variable.t -> ('a Variable.t -> 'b cc) -> 'b cc
