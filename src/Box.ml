open Basis
open Flags

type unique_gen
type unique

type index = unique
type 'a box = { label: unique; value: 'a }

type 'a t = 'a box

external ugen_with_label: string -> unique_gen = "Unique" [@@bs.new] [@@bs.module "boost/dist/esm/ds/index.js"]

external uniq: unique_gen -> unique = "value" [@@bs.get]

external as_uniq: string -> unique = "%identity"

external uniq_to_string: unique -> string = "%identity"

let getLabel ?(label="loc") () =
   let gen = (ugen_with_label label) in
   uniq gen


let init () = ()

let prim k n = as_uniq (k ^ string_of_int n)

let inspect box ~depth:_ ~options =
  let child = inspectWithOptions box.value options in
  let label = options.stylize (uniq_to_string box.label) "special" in
  Format.sprintf "{ %s: %s }" label child

let create ?label v =
  let label = getLabel ?label () in
  let box = { label; value = v } in
  if pretty_output then setInspector box (inspect box) else box

let fromPrim k value =
  let label = prim k (hash value) in
  let box = { label; value } in
  if pretty_output then setInspector box (inspect box) else box

let fromInt i =
  let label = prim "%i" i in
  let box = { label; value = i } in
  if pretty_output then setInspector box (inspect box) else box

let fromOption ob =
  match ob with
    | None -> fromPrim "%o" ob
    | Some( { label }) ->
      let box = { label; value = ob } in
      if pretty_output then setInspector box (inspect box) else box

let eq {label = ka} {label = kb} = ka == kb

let valueOf { value } = value

let indexOf { label } = label

let fromString a = fromPrim "%s" a
