open Basis
open Flags
open Boost
include Types_internal

type t = time

exception BadNode

let list = ref (Linked_list.make ())
  
let now () = { at = if real_time then Js.Date.now () else 0.; sub = 0.; isSplicedOut = false }

let getNext ({ next }: t) = next

let create () =
  let timestamp = now () in
  Linked_list.add_to_end (!list) timestamp

let compare = fun (a: t) (b: t) ->
  if a == b then Equal else
  match Compare.exec Compare.num a.value.at b.value.at with
    | Equal -> Compare.exec Compare.num a.value.sub b.value.sub
    | cmp -> cmp

external __unsafe_inline : float -> string = "%identity"

external toArray: 'a Linked_list.t -> 'a array = "from" [@@bs.val][@@bs.scope "Array"]

let toString (t: t) = ((__unsafe_inline t.value.at) ^ "|") ^ (__unsafe_inline t.value.sub)

let describeTime () = Js.log (toArray (!list))

let setInspector t = Inspect.setInspector t (Inspect.time t)

let add (a: t) =
  match a.next with
    | Some(next) ->
      if a.value.at == next.value.at then
        let sub = (a.value.sub +. next.value.sub) /. 2. in
        let timestamp = { at = a.value.at; sub; isSplicedOut = false } in
        Linked_list.add_after (!list) a timestamp |> setInspector
      else
        let at = ((a.value.at +. next.value.at) /. 2.) in
        let timestamp = { at; sub = 0.; isSplicedOut = false } in
        Linked_list.add_after (!list) a timestamp |> setInspector
    | None ->
      let at = if real_time then Js.Date.now () else 0. in
      if at = a.value.at then
        let timestamp = { at; sub = a.value.sub +. 1.; isSplicedOut = false } in
        Linked_list.add_to_end (!list) timestamp |> setInspector
      else
        let timestamp = { at; sub = 0.; isSplicedOut = false } in
        Linked_list.add_to_end (!list) timestamp |> setInspector

let init () =
  list := Linked_list.make ()

let spliceOut (start: t) (stop: t) =
  let _ = if Flags.debug_propagate then
    Js.log4 "Time.spliceOut from" start "to" stop
  in
  let rec deleteRange (next: t) =
    if next == stop then ()
    else
      let nextnext = next.next in
      Linked_list.remove_node (!list) next;
      next.value.isSplicedOut <- true;
      match nextnext with
        | Some(n) -> 
          deleteRange n
        | None -> raise BadNode
  in
  match compare start stop with
    | Less -> deleteRange(start)
    | _ -> ()

let isSplicedOut ({ value={ isSplicedOut } }: t) = isSplicedOut

let compareWindow (l, _) (r, _) = compare l r

let inspectTime () = Inspect.custom Inspect.linkedList (!list)
