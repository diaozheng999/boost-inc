include Inspect_internal
include InspectRe

let list inspectElement list ~depth ~options =
  match list with
    | [] -> options##stylize "Nil" `special
    | _::_ ->
      if depth < 0 then options##stylize "[List]" `special else
        let arr = Belt.List.toArray list in
        let toInspect = Belt.Array.map arr
          (fun v -> setInspectObject (inspectElement v)) in
        let r = withOptions toInspect ~options   
      in r |> Js.String.replaceByRe [%bs.re "/\\n/g"] "\n  "

let log1 format a = 
  let formatted_a = inspect a in
  Js.log (Printf.sprintf format formatted_a)

let log2 format a b =
  let formatted_a = inspect a in
  let formatted_b = inspect b in
  Js.log (Printf.sprintf format formatted_a formatted_b)

let log3 format a b c =
  let formatted_a = inspect a in
  let formatted_b = inspect b in
  let formatted_c = inspect c in
  Js.log (Printf.sprintf format formatted_a formatted_b formatted_c)

let log4 format a b c d =
  let formatted_a = inspect a in
  let formatted_b = inspect b in
  let formatted_c = inspect c in
  let formatted_d = inspect d in
  Js.log (Printf.sprintf format formatted_a formatted_b formatted_c formatted_d)
