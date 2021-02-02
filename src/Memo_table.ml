type 'a entry = ('a * Time.window option) option ref

type ('k, 'v) js_map

type 'a memotable = (int, 'a entry) js_map
type 'a t = 'a memotable

external create: unit -> 'a memotable = "Map" [@@bs.new]

external get: ('k, 'v) js_map -> 'k -> 'v option = "get" [@@bs.send]

let rec find' table list time current =
  match list with
    | [] -> current
    | k::ks ->
      match get table k with
        | None -> find' table ks time current
        | Some(e) ->
          match !e with
            | Some(_, Some (t, _)) -> (
              match Time.compare t time with
                | CmpImpl.Less -> find' table ks t e
                | _ -> find' table ks time current
            )
            | _ -> find' table ks time current 

let find table list time = find' table list time (ref None)
