open Basis
open Combinators

type 'a var = {
  modref: 'a Box.t modref;
  change: 'a -> unit;
  create: 'a -> 'a Box.t;
  eq: 'a Box.t eq;
  changeEagerly: 'a -> unit;
}

type 'a t = 'a var

let change ({ modref; create; eq }: 'a var) a =
  let next = create a in
  Modifiable.change' eq modref next 

let changeEagerly v a =
  change v a;
  Modifiable.propagate ()

let createVarFromModref ?(eq=Box.eq) create modref =
  let rec v = {
    modref;
    change = (fun a -> change v a);
    changeEagerly = (fun a -> changeEagerly v a);
    create;
    eq;
  } in v

let createVar ?(eq=Box.eq) create v =
  let modref = Modifiable.create (create v) in
  createVarFromModref ~eq create modref

let create ?(label="var") = createVar (Box.create ~label)

let int = createVar (Box.fromInt)

let opt o = createVar (Box.fromOption) o

let str = createVar (Box.fromString)

let ofCombinator (r: 'a cc) =
  let inst = modref r in
  createVarFromModref ~eq:Box.eq (Box.create ~label:"cc") inst

let observe ~f (v: 'a var) = Modifiable.observe v.modref f

let log v = observe ~f:Js.Console.log v
