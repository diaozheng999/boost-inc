open Flags

type 'a entry = ('a * Time.window option) option ref

type ('k, 'v) js_map

type 'a memotable = (Box.index, 'a entry list) js_map
type 'a t = 'a memotable

external createInternal: unit -> 'a memotable = "Map" [@@bs.new]

external get: ('k, 'v) js_map -> 'k -> 'v option = "get" [@@bs.send]

external set': ('k, 'v) js_map -> 'k -> 'v -> unit = "set" [@@bs.send]

external delete: ('k, 'v) js_map -> 'k -> unit = "delete" [@@bs.send]

external setOneUnsafe: ('k, 'v) js_map -> string -> Box.unique -> unit = "set" [@@bs.send]

external getUnsafe: ('k, 'v) js_map -> string -> string = "get" [@@bs.send]

external getOptUnsafe: ('k, 'v) js_map -> string -> string option = "get" [@@bs.send]

external forEach: ('k, 'v) js_map -> ('v -> 'k -> unit [@bs.uncurry 2]) -> unit = "forEach" [@@bs.send]

let reduce m ~filter f init =
  let acc = ref init in
  forEach m (fun v k -> if filter k then acc := (f (!acc) k v));
  !acc

let inspect t ~depth ~options =
  let instance = options##stylize (getUnsafe t "$$instance") `undefined in
  let name = getOptUnsafe t "$$name" in
  let title = match name with
    | None -> Printf.sprintf "Memotable (%s)" instance
    | Some name -> Printf.sprintf "%s (%s)" (options##stylize name `special) instance
  in
  if depth < 0 then
    options##stylize title `special
  else
    let childDepth = depth - 1 in
    let childOptions = Inspect.reduceDepth options in
    let filter k =
      k != Box.as_uniq "$$instance" && k != Box.as_uniq "$$name"
    in

    let inspectChild acc k v =
      let formattedK = options##stylize (Box.uniq_to_string k) `string in
      let formattedV = Inspect.list Inspect.memotableEntry v ~depth:childDepth ~options:childOptions  in
      Format.sprintf "%s\n  %s => %s" acc formattedK formattedV
    in

    let values = reduce t ~filter inspectChild "" in
    Printf.sprintf "%s { %s\n}" title values

  
let create ?name () =
  let map = createInternal () in
  setOneUnsafe map "$$instance" (Box.getLabel ~label:"pad" ());
  (match name with
    | Some(value) -> setOneUnsafe map "$$name" (Box.as_uniq value)
    | _ -> ());
  if pretty_output then Inspect.setInspector map (inspect map) else map

let set (table: 'a t) key value =
  match !value with
    | None -> ()
    | Some _ ->
      match get table key with
        | None -> set' table key [value]
        | Some existing -> set' table key (value::existing)

(**
  Finds the node in the list with the earliest start time, and at the same time
  removes nodes with spliced out windows accumulated in TCO parameter `next`.
 *)
let rec find' list time current next =

  let _ = if debug_combinators then
    let lst = Inspect.custom (Inspect.list Inspect.memotableEntry) list in 
    Js.log4 "Memo_table.find': current list" lst time current in
  match list with
    | [] -> current, next
    | e::ks ->
      match !e with
        | Some(_, Some(t, u)) -> (
            let n =
              if Time.isSplicedOut t && Time.isSplicedOut u then
                next
              else e::next
            in 
            match Time.compare t time with
              | CmpImpl.Less -> find' ks t e n
              | _ -> find' ks time current n
          )
        | Some(_, None) -> e, next
        |  _ -> find' ks time current next

let find table key time =
  if debug_combinators then
    Js.log4 "Memo_table.find: retrieving from" key time table;
  match get table key with
    | None ->
      if debug_combinators then
        Js.log "Memo_table.find: not found.";
      ref None
    | Some list -> 
      let result, reduced = find' list time (ref None) [] in
      let _ = match reduced with
        | [] -> delete table key
        | _ -> set' table key reduced
      in
      result
