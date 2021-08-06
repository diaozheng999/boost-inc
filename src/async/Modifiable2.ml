open Boost

type t = unit Js.Promise.t

type 'a cc = 'a Changeable.t

let create_gen = Boost.Unique.make_with_label ~label:"Modifiable2$create"

let read_gen = Boost.Unique.make_with_label ~label:"Modifiable2$read"

let write_gen = Boost.Unique.make_with_label ~label:"Modifiable2$write"

let variable c =
  let result = Variable.make_intermediate () in
  let (_ : unit Js.Promise.t) = Changeable.act c result in
  result

let create v = Variable_prim.mem ~name:(Boost.Unique.string create_gen) v

let read variable next = Changeable.read ~label:(Unique.value read_gen) variable next

let write a = Changeable.write ~label:(Unique.value write_gen) a

let mk_lift = Memo.mk_lift

let mk_lift_cc ~name = Memo.mk_lift_cc ~name ()
