open Basis

type unique_gen
type unique

type index = unique
type 'a box = { label: unique; value: 'a }

type 'a t = 'a box

external ugen_with_label: string -> unique_gen = "Unique" [@@bs.new] [@@bs.module "boost/dist/esm/ds/index.js"]

external uniq: unique_gen -> unique = "value" [@@bs.get]

external as_uniq: string -> unique = "%identity"

let getLabel ?(label="loc") () =
   let gen = (ugen_with_label label) in
   uniq gen


let init () = ()

let prim k n = as_uniq (k ^ string_of_int n)

let create ?label v =
  let label = getLabel ?label () in
  { label; value = v }

let fromPrim k value =
  let label = prim k (hash value) in
  { label; value }

let fromInt i =
  let label = prim "%i" i in
  { label; value = i }

let fromOption ob =
  match ob with
    | None -> fromPrim "%o" ob
    | Some( { label }) -> { label; value = ob }

let eq {label = ka} {label = kb} = ka == kb

let valueOf { value } = value

let indexOf { label } = label

let fromString = fromPrim "%s"
