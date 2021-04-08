open Var

external pmap : 'a Js.Promise.t -> ('a -> 'b) -> 'b Js.Promise.t = "then"
  [@@bs.send]

let assign promise v =
  pmap promise v.changeEagerly |> ignore;
  v

let empty p =
  let result = Var.make ~label:"promise" empty in
  assign p result

let int p =
  let result = Var.int 0 in
  assign p result

let str p =
  let result = Var.str "" in
  assign p result

let opt p =
  let result = Var.opt None in
  assign p result

let ofJS p defaultValue =
  let result = Var.make_assuming_same_type defaultValue in
  assign p result

let make p defaultValue =
  let result = Var.make ~label:"promise" defaultValue in
  assign p result
