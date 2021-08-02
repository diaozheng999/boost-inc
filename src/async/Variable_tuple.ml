open Boost
open Variable

let gen = Unique.make_with_label ~label:"tuple"

let make_label ?name variables =
  let label =
    Unique.to_str
      (match name with
      | Some value -> Unique.value value
      | None -> Unique.value gen)
  in
  let concatenated = Js.Array2.joinWith variables ", " in
  {j|$(label)[$(concatenated)]|j} |> Unique.of_str

let hash_tuple arr = Hash.hash_array arr |> Address.int

let make2 ?name v1 v2 =
  let name = make_label ?name [| v1.name; v2.name |] in
  let compute_address =
   fun [@bs] (a, b) ->
    hash_tuple [| (v1.compute_address a [@bs]); (v2.compute_address b [@bs]) |]
  in
  let last_value =
    match (v1.last_value, v2.last_value) with
    | Some v1, Some v2 -> Some (v1, v2)
    | _ -> None
  in
  let read =
   fun [@bs] () ->
    Js.Promise.all2 ((v1.read () [@bs]), (v2.read () [@bs]))
    |> Js.Promise.then_ (fun ((a, addr_a), (b, addr_b)) ->
           let addr = hash_tuple [| addr_a; addr_b |] in
           Js.Promise.resolve ((a, b), addr))
  in

  let write =
   fun [@bs] _ (a, b) ->
    let addr_a = (v1.compute_address a [@bs]) in
    let addr_b = (v2.compute_address b [@bs]) in
    Js.Promise.all2 ((v1.write addr_a a [@bs]), (v2.write addr_b b [@bs]))
    |> Js.Promise.then_ (fun _ -> Js.Promise.resolve ())
  in

  make ~read ~write ~compute_address ?last_value name

let make3 ?name v1 v2 v3 =
  let name = make_label ?name [| v1.name; v2.name; v3.name |] in
  let compute_address =
   fun [@bs] (a, b, c) ->
    hash_tuple
      [|
        (v1.compute_address a [@bs]);
        (v2.compute_address b [@bs]);
        (v3.compute_address c [@bs]);
      |]
  in
  let last_value =
    match (v1.last_value, v2.last_value, v3.last_value) with
    | Some v1, Some v2, Some v3 -> Some (v1, v2, v3)
    | _ -> None
  in
  let read =
   fun [@bs] () ->
    Js.Promise.all3 ((v1.read () [@bs]), (v2.read () [@bs]), (v3.read () [@bs]))
    |> Js.Promise.then_ (fun ((a, addr_a), (b, addr_b), (c, addr_c)) ->
           let addr = hash_tuple [| addr_a; addr_b; addr_c |] in
           Js.Promise.resolve ((a, b, c), addr))
  in

  let write =
   fun [@bs] _ (a, b, c) ->
    let addr_a = (v1.compute_address a [@bs]) in
    let addr_b = (v2.compute_address b [@bs]) in
    let addr_c = (v3.compute_address c [@bs]) in
    Js.Promise.all3
      ( (v1.write addr_a a [@bs]),
        (v2.write addr_b b [@bs]),
        (v3.write addr_c c [@bs]) )
    |> Js.Promise.then_ (fun _ -> Js.Promise.resolve ())
  in

  make ~read ~write ~compute_address ?last_value name
