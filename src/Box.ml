open Basis
open Flags
open Boost.Unique

type index = unique
type 'a box = { label: index; value: 'a }

type 'a t = 'a box

let getLabel ?(label="loc") () =
   let gen = (makeWithLabel ~label) in
   value gen


let init () = ()

let prim k n = fromString (k ^ string_of_int n)

let inspect box ~depth:_ ~options =
  let child = Inspect.withOptions box.value ~options in
  let label = options##stylize (toString box.label) `string in
  Format.sprintf "[Box %s: %s ]" label child

let create ?label v =
  let label = getLabel ?label () in
  let box = { label; value = v } in
  if pretty_output then Inspect.setInspector box (inspect box) else box

let fromPrim k value =
  let label = prim k (hash value) in
  let box = { label; value } in
  if pretty_output then Inspect.setInspector box (inspect box) else box

let fromInt i =
  let label = prim "%i" i in
  let box = { label; value = i } in
  if pretty_output then Inspect.setInspector box (inspect box) else box

let fromOption ob =
  match ob with
    | None -> fromPrim "%o" ob
    | Some( { label }) ->
      let box = { label; value = ob } in
      if pretty_output then Inspect.setInspector box (inspect box) else box

let eq {label = ka} {label = kb} = ka == kb

let valueOf { value } = value

let indexOf { label } = label

let fromString a = fromPrim "%s" a
