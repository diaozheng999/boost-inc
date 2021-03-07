type 'a emitter = {
  emit: 'a -> unit;
  var: 'a Var.t;
  loc: Yalib.Unique.t;
  modref: 'a Box.t Modifiable.t;
}

val make: unit -> 'a emitter

val makeWithLabel: string -> 'a emitter

val modref: 'a emitter -> 'a Box.t Modifiable.t
