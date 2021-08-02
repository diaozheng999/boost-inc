open Boost

let gen = Unique.make_with_label ~label:"prim"

let prim ?name b =
  let name = Basis.get_uniq_with_default ?label:name gen in
  let compute_address = Bs_interop.arity1 Address.unsafe_prim_to_addr in
  let last_value = b in
  let last_address = Address.unsafe_prim_to_addr b in
  Variable.make ~compute_address ~last_value ~last_address name

let make ?name compute_address last_value =
  let name = Basis.get_uniq_with_default ?label:name gen in
  Variable.make ~compute_address ~last_value name

let int = prim

let bool = prim

let str = prim

let float = prim

let compute_address_opt =
 fun [@bs] a ->
  match a with None -> Address.none_addr | Some a -> Address.int (Hash.hash a)

let opt ?name value = make ?name compute_address_opt value

let mem ?name value = make ?name Variable.compute_address_prim value
