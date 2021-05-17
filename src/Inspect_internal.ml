(** Inspect_internal.ml - contains internal operations for `util.inspect` used
    internally by both `InspectRe.res` and `Inspect.ml` *)

(**
  NodeJS's stylize options. "module" is excluded.

  Use `stylizeModule` instead.
 *)
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

(** Inspect options, use `options##stylize` in OCaml and `options["stylize"]`
    in ReScript *)
type node_js_inspect_options = <
  stylize: (string -> stylize_options -> string) [@bs.meth];
  depth: int;
  colors: bool;
> Js.t

(* A custom inspector that prints the object to console. Usually used as
   `'a -> 'a inspector` *)
type 'a inspector = depth:int -> options:node_js_inspect_options -> string

(******************************************************************************)
(** This section defines JavaScript FFI interfaces to attach the
    `util.inspect.custom` symbol to a (potentially JavaScript) object. In this
    way, we can pretty-print our data structures. This entire module is not
    used by default if `Flags.pretty_output` is set to `false`. *)

(** An abstract type representing the `util.inspect.custom` symbol *)
type inspect_symbol

(** The `util.inspect.custom` symbol *)
external inspect_symbol: inspect_symbol = "custom" [@@bs.module "util"] [@@bs.scope "inspect"]

external inspect: 'a -> string = "inspect" [@@bs.module "util"]

external withOptions: 'a -> options:node_js_inspect_options -> string = "inspect" [@@bs.module "util"]

external defaultOptions: node_js_inspect_options = "defaultOptions" [@@bs.module "util"][@@bs.scope "inspect"]

external magic: string -> stylize_options = "%identity"

(** Does the equivalent of the following JavaScript call:
    ```
    a[util.inspect.custom] = f
    ``` *)
let unsafe_setInspectFunction (a: 'a) (f: 'a inspector) (c: inspect_symbol) : unit =
  [%bs.raw "a[c] = f"]
[@@warning "-27"]

(** Sets the inspector of an object and returns it *)
let setInspector a insp =
  let () =
    if Flags.inspect_with_polyfill then
      Js.Dict.set (Obj.magic a) "inspect" insp
    else
      unsafe_setInspectFunction a insp inspect_symbol
  in a

(******************************************************************************)

let stylizeModule ~options value =
  options##stylize value (magic "module")

let reduceDepth options =
  let copy = Js.Obj.assign [%obj { __boost_ReducedByInspector=true }] options in
  let depth = options##depth - 1 in
  Js.Obj.assign copy [%obj { depth }]


let withDefault values =
  let copy = Js.Obj.assign [%obj { __boost_InducedByInspector=true }] defaultOptions in
  Js.Obj.assign copy values

let stylizeWith options kind =
  let copy = Js.Obj.assign [%obj { __boost_ReassignedByInspector=true }] options in
  let stylize = (fun str _ -> options##stylize str kind) [@bs] in
  Js.Obj.assign copy [%obj { stylize }]

let setInspectObject insp = setInspector (Js.Obj.empty ()) insp [%%private]
 
let default a ~depth:_ ~options = withOptions a ~options

let with_ inspector elem ~options =
  match Js.Types.classify elem with
    | Js.Types.JSObject _
    | Js.Types.JSFunction _ ->
      let _ = setInspector elem (inspector elem) in
      withOptions elem ~options
    | _ ->
      let obj = setInspectObject (inspector elem) in
      withOptions obj ~options

let custom inspector elem =
  let options = withDefault [%obj { colors=true }] in
  if Flags.pretty_output then
    with_ inspector elem ~options
  else
    withOptions elem ~options
