open Flags

(* NodeJS's stylize options. "module" is excluded. *)
type stylize_options = [
  | `bigint
  | `boolean
  | `date
  | `name
  | `null
  | `number
  | `regexp
  | `special
  | `string
  | `symbol
  | `undefined
]

type node_js_inspect_options = <
  stylize: (string -> stylize_options -> string) Js_OO.Meth.arity2;
  depth: int;
  colors: bool;
> Js.t

type 'a inspector = depth:int -> options:node_js_inspect_options -> string

type 'a inspected

type inspect_symbol

external custom: inspect_symbol = "custom" [@@bs.module "util"] [@@bs.scope "inspect"]

external inspect: 'a -> string = "inspect" [@@bs.module "util"]

external withOptions: 'a -> options:node_js_inspect_options -> string = "inspect" [@@bs.module "util"]

external defaultOptions: node_js_inspect_options = "defaultOptions" [@@bs.module "util"][@@bs.scope "inspect"]

external magic: string -> stylize_options = "%identity" [%%private]

let unsafe_setInspectFunction (a: 'a) (f: 'a inspector) (c: inspect_symbol) =
  [%bs.raw "a[c] = f"]
[@@warning "-27"]

let setInspector a insp =
  unsafe_setInspectFunction a insp custom |> ignore; a [%%private]

let stylizeModule ~options value =
  options##stylize value (magic "module")

let reduceDepth options =
  let copy = Js.Obj.assign [%obj { __boost_ReducedByInspector=true }] options in
  let depth = options##depth - 1 in
  Js.Obj.assign copy [%obj { depth }]


let withDefault values =
  let copy = Js.Obj.assign [%obj { __boost_InducedByInspector=true }] defaultOptions in
  Js.Obj.assign copy values

let setInspectObject insp = setInspector (Js.Obj.empty ()) insp [%%private]
 
let default a ~depth:_ ~options = withOptions a ~options

let with_ inspector elem ~options =
  match Js.Types.classify elem with
    | Js.Types.JSObject _
    | Js.Types.JSFunction _ ->
      let _ = setInspector elem (inspector elem) in
      withOptions elem ~options
    | _ ->
      let _ = setInspectObject (inspector elem) in
      withOptions elem ~options

let custom inspector elem =
  let options = withDefault [%obj { colors=true }] in
  if Flags.pretty_output then
    with_ inspector elem ~options
  else
    withOptions elem ~options