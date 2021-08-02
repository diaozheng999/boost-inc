open Boost

external unsafe_prim_to_addr : 'a -> Address.t = "%identity"

let none_addr : Address.t = [%raw "Symbol.for('boost-inc/none')"]

let gen = Unique.make_with_label ~label:"prim"


let prim ?name b =
  let name = Basis.get_uniq_with_default ?label:name gen in
  let compute_address = Bs_interop.arity1 unsafe_prim_to_addr in
  let last_value = b in
  let last_address = (unsafe_prim_to_addr b) in
  Variable.make ~compute_address ~last_value ~last_address name

let make  ?name compute_address last_value =
  let name = Basis.get_uniq_with_default ?label:name gen in
  Variable.make ~compute_address ~last_value name

let int = prim

let bool = prim

let str = prim

let float = prim

let compute_address_opt = fun [@bs] a ->
  match a with
    | None -> none_addr
    | Some a -> Address.int (Hash.hash a)

let compute_address_prim = fun [@bs] v ->
  match Js.typeof v with
    | "undefined" -> none_addr
    | "number"
    | "string"
    | "boolean"
    | "symbol" -> unsafe_prim_to_addr v
    | _ -> Hash.hash v |> Address.int

let opt ?name value = make ?name compute_address_opt value

let mem ?name value = make ?name compute_address_prim value
