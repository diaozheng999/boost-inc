type 'a entry = ('a * Time.window option) option ref

type ('k, 'v) js_map

type 'a memotable = (Box.index, 'a entry list) js_map
type 'a t = 'a memotable

external createInternal: unit -> 'a memotable = "Map" [@@bs.new]

external get: ('k, 'v) js_map -> 'k -> 'v option = "get" [@@bs.send]

external set': ('k, 'v) js_map -> 'k -> 'v -> unit = "set" [@@bs.send]

external setOneUnsafe: ('k, 'v) js_map -> string -> Box.unique -> unit = "set" [@@bs.send]

let create () =
  let map = createInternal () in
  setOneUnsafe map "$$instance" (Box.getLabel ~label:"pad" ());
  map

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
       |  _ -> find' ks time current

let find table key time =
  match get table key with
    | None -> ref None
    | Some list -> find' list time (ref None)
