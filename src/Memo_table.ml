open Basis
open Flags

type 'a entry = ('a * Time.window option) option ref

type ('k, 'v) js_map

type 'a memotable = (Box.index, 'a entry list) js_map
type 'a t = 'a memotable

external createInternal: unit -> 'a memotable = "Map" [@@bs.new]

external get: ('k, 'v) js_map -> 'k -> 'v option = "get" [@@bs.send]

external set': ('k, 'v) js_map -> 'k -> 'v -> unit = "set" [@@bs.send]

external setOneUnsafe: ('k, 'v) js_map -> string -> Box.unique -> unit = "set" [@@bs.send]

external getUnsafe: ('k, 'v) js_map -> string -> string = "get" [@@bs.send]

external getOptUnsafe: ('k, 'v) js_map -> string -> string option = "get" [@@bs.send]

external forEach: ('k, 'v) js_map -> ('v -> 'k -> unit [@bs.uncurry 2]) -> unit = "forEach" [@@bs.send]

let reduce m ~filter f init =
  let acc = ref init in
  forEach m (fun v k -> if filter k then acc := (f (!acc) k v));
  !acc

let rec inspectElement e ~depth ~options =
  if depth < 0 then options.stylize "[reader]" "special" else
    match !e with
    | Some (_, None) ->
        options.stylize "undefined" "undefined"
        |> Format.sprintf "reader at %s time" 
    | Some (_, Some (f, t)) ->
        let f = inspectWithOptions f options in
        let t = inspectWithOptions t options in
        Format.sprintf "reader from %s to %s" f t
    | _ -> options.stylize "spliced out" "undefined"


let inspect t ~depth ~options =
  let instance = options.stylize (getUnsafe t "$$instance") "undefined" in
  let name = getOptUnsafe t "$$name" in
  let title = match name with
    | None -> Printf.sprintf "Memotable (%s)" instance
    | Some name -> Printf.sprintf "%s (%s)" (options.stylize name "special") instance
  in
  let childDepth = depth - 1 in
  let childOptions = reduceDepth options in
  let filter k =
    k != Box.as_uniq "$$instance" && k != Box.as_uniq "$$name"
  in

  let inspectChild acc k v =
    let formattedK = options.stylize (Box.uniq_to_string k) "string" in
    let formattedV = inspectList inspectElement v ~depth:childDepth ~options:childOptions  in
    Format.sprintf "%s\n  %s => %s" acc formattedK formattedV
  in

  let values = reduce t ~filter inspectChild "" in
  title ^ values

  
let create ?name () =
  let map = createInternal () in
  setOneUnsafe map "$$instance" (Box.getLabel ~label:"pad" ());
  (match name with
    | Some(value) -> setOneUnsafe map "$$name" (Box.as_uniq value)
    | _ -> ());
  if pretty_output then setInspector map (inspect map) else map

let set (table: 'a t) key value =
  match !value with
    | None -> ()
    | Some _ ->
      match get table key with
        | None -> set' table key [value]
        | Some existing -> set' table key (value::existing)

let rec find' list time current =
  match list with
    | [] -> current
    | e::ks ->
      match !e with
        | Some(_, Some(t, _)) -> (
            match Time.compare t time with
              | CmpImpl.Less -> find' ks t e
              | _ -> find' ks time current
          )
        | Some(_, None) -> e
        |  _ -> find' ks time current

let find table key time =
  match get table key with
    | None -> ref None
    | Some list -> find' list time (ref None)
