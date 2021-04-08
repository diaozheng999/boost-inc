open Yalib

type 'a emitter = {
  emit: 'a -> unit;
  var: 'a Var.t;
  loc: Unique.t;
  modref: 'a Types_internal.box Modifiable.t;
}

let make' ~label =
  let gen = Unique.make_with_label ~label in
  let loc = Unique.value gen in
  let label = Format.sprintf "%s_emit" (Unique.to_str loc) in
  let variable = Var.empty ~label () in
  {
    loc;
    emit = variable.changeEagerly;
    var = variable;
    modref = variable.modref;
  }

let make () = make' ~label:"emitter"

let makeWithLabel label = make' ~label

let modref { modref } = modref
