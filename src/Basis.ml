include CmpImpl

type node_js_inspect_options = {
  stylize: string -> string -> string;
  depth: int;
}

type 'a inspector = depth:int -> options:node_js_inspect_options -> string

type 'a inspected

type inspect_symbol

exception Inspect

let rec eq al bl =
    match (al, bl) with
      | ([], []) -> true
      | (_::_, []) -> false
      | ([], _::_) -> false
      | (a::a', b::b') -> 
        if a = b then eq a' b' else false

external toString: 'a -> string = "toString" [@@bs.send]

external hash: 'a -> int = "hash" [@@bs.module "boost/dist/esm/common/index.js"]

external custom: inspect_symbol = "custom" [@@bs.module "util"] [@@bs.scope "inspect"]

external inspect: 'a -> string = "inspect" [@@bs.module "util"]

external inspectWithOptions: 'a -> node_js_inspect_options -> string = "inspect" [@@bs.module "util"]

external inspectDefaultOptions: node_js_inspect_options = "defaultOptions" [@@bs.module "util"][@@bs.scope "inspect"]

let unsafe_setInspectFunction (a: 'a) (f: 'a inspector) (c: inspect_symbol) =
  [%bs.raw "a[c] = f"]
[@@warning "-27"]

let setInspector a insp =
  unsafe_setInspectFunction a insp custom |> ignore; a [%%private]

let reduceDepth { stylize; depth } = { stylize; depth = depth - 1 }


let inspectRef inspectElement ref  ~depth ~options =
  let deref = inspectElement (!ref) ~depth ~options in
  Format.sprintf "%s %s" (options.stylize "#ref" "undefined") deref

let setInspectObject insp = setInspector (Js.Obj.empty ()) insp

let inspectList inspectElement list ~depth ~options =
  if depth < 0 then options.stylize "[List]" "special" else
    let arr = Belt.List.toArray list in
    let toInspect = Belt.Array.map arr
      (fun v -> setInspectObject (inspectElement v)) in
    inspectWithOptions toInspect options
      |> Js.String.replaceByRe [%bs.re "/\\n/g"] "\n  "

let inspectOpt inspectElement opt ~depth ~options =
  if depth < 0 then options.stylize "[Option]" "special" else
    match opt with
      | None -> options.stylize "None" "undefined"
      | Some v -> inspectElement v ~depth ~options |> Printf.sprintf "Some(%s)"

let inspectDefault a ~depth ~options =
    if depth != options.depth then raise Inspect;
  inspectWithOptions a options
