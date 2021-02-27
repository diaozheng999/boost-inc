open Basis
open Flags
open Boost.Unique
open Types_internal

type unique = Boost.Unique.t
type index = unique
type 'a box = 'a Types_internal.box

type 'a t = 'a box

let getLabel ?(label="loc") () =
   let gen = (makeWithLabel ~label) in
   value gen

let init () = ()

let prim k n = fromString (k ^ string_of_int n)

let as_uniq = Boost.Unique.fromString
let uniq_to_string = Boost.Unique.toString

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

let fromOption ob : 'a option box=
  match ob with
    | None -> fromPrim "%o" None
    | Some value ->
      let box = create (Some value) in
      if pretty_output then Inspect.setInspector box (inspect box) else box

let eq ({label = ka} as ba) ({label = kb} as bb) = ba == bb || ka == kb

let valueOf { value } = value

let indexOf { label } = label

let fromString (a: string) = fromPrim "%s" a
