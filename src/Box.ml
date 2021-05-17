open Basis
open Flags
open Boost.Unique
open Types_internal

type unique = Boost.Unique.t

type index = unique

type 'a box = 'a Types_internal.box

type 'a t = 'a box

let getLabel ?(label = "loc") () =
  let gen = make_with_label ~label in
  value gen

let init () = ()

let prim k n = of_str (k ^ string_of_int n)

let as_uniq = Boost.Unique.of_str

let uniq_to_string = Boost.Unique.to_str

let inspect box ~depth:_ ~options =
  let child = Inspect.withOptions box.value ~options in
  let label = options##stylize (to_str box.label) `string in
  Format.sprintf "[Box %s: %s ]" label child

let create ?label v =
  let label = getLabel ?label () in
  let box = { label; value = v } in
  if pretty_output then Inspect.setInspector box (inspect box) else box

let of_prim k value =
  let label = prim k (hash value) in
  let box = { label; value } in
  if pretty_output then Inspect.setInspector box (inspect box) else box

let of_int i =
  let label = prim "%i" i in
  let box = { label; value = i } in
  if pretty_output then Inspect.setInspector box (inspect box) else box

let with_custom_hash_function ~hash value =
  let label = as_uniq (hash value) in
  let box = { label; value } in
  if pretty_output then Inspect.setInspector box (inspect box) else box

let copy box =
  let { label; value } = box in
  create ~label:(Boost.Unique.to_str label ^ "_copy") value

let of_opt ob : 'a option box =
  match ob with
  | None -> of_prim "%o" None
  | Some value ->
      let box = create (Some value) in
      if pretty_output then Inspect.setInspector box (inspect box) else box

let eq ({ label = ka } as ba) ({ label = kb } as bb) = ba == bb || ka == kb

let mk_eq eqv ({ label = ka; value = va } as ba) ({ label = kb; value = vb } as bb) =
  ba == bb || ka == kb || eqv va vb

let value { value } = value

let idx { label } = label

let of_str (a : string) = of_prim "%s" a

(** Deprecated interfaces *)

let fromOption = of_opt [@@deprecated "Renamed to of_opt"]

let valueOf = value [@@deprecated "Renamed to value"]

let indexOf = idx [@@deprecated "Renamed to idx"]

let fromString = of_str [@@deprecated "Renamed to of_str"]

let fromPrim = of_prim [@@deprecated "Renamed to of_prim"]

let fromInt = of_int [@@deprecated "Renamed to of_int"]

let withCustomHashFunction = with_custom_hash_function  [@@deprecated "Renamed to with_custom_hash_function"]
