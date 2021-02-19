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
