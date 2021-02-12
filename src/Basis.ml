include CmpImpl

type node_js_inspect_options = {
  stylize: string -> string -> string;
  depth: int;
}

type 'a inspector = depth:int -> options:node_js_inspect_options -> string

type 'a inspected

type inspect_symbol

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

let unsafe_setInspectFunction (a: 'a) (f: 'a inspector) (c: inspect_symbol) =
  [%bs.raw "a[c] = f"]
[@@warning "-27"]

let setInspector a insp =
  unsafe_setInspectFunction a insp custom |> ignore; a [%%private]

let reduceDepth { stylize; depth } = { stylize; depth = depth - 1 }


let inspectRef ref ~depth:_ ~options =
  let deref = inspectWithOptions (!ref) options in
  Format.sprintf "%s %s" (options.stylize "#ref" "undefined") deref
