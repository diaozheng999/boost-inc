open Boost

type 'a emitter = {
  loc: Unique.t;
  emit: 'a -> unit;
  var: 'a Var.t;
  __modref: 'a Types_internal.box Modifiable.t;
}

let make' ~label =
  let gen = Unique.makeWithLabel ~label in
  let loc = Unique.value gen in
  let label = Format.sprintf "%s_emit" (Unique.toString loc) in
  let variable = Var.empty ~label () in
  {
    loc;
    emit = variable.changeEagerly;
    var = variable;
    __modref = variable.modref;
  }

let make () = make' ~label:"emitter"

let makeWithLabel label = make' ~label
