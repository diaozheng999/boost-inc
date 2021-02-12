open Basis
open Flags

type timestamp = {
  at: float;
  sub: float;
  mutable isSplicedOut: bool;
}

type t = timestamp LinkedListImpl.boost_linked_list_node
type window = t * t

exception BadNode

let list = ref (LinkedListImpl.init ())
  
let now () = { at = if real_time then Js.Date.now () else 0.; sub = 0.; isSplicedOut = false }

let getNext ({ next }: t) = next

let create () =
  let timestamp = now () in
  LinkedListImpl.addToEnd (!list) timestamp

let compare (a: t) (b: t) =
  if a == b then Equal else
  match ordFromJs (num a.value.at b.value.at) with
    | Equal -> ordFromJs (num a.value.sub b.value.sub)
    | cmp -> cmp

external __unsafe_inline : float -> string = "%identity"

external toArray: 'a LinkedListImpl.t -> 'a array = "from" [@@bs.val][@@bs.scope "Array"]

let toString (t: t) = ((__unsafe_inline t.value.at) ^ "|") ^ (__unsafe_inline t.value.sub)

let describeTime () = Js.log (toArray (!list))

let inspect (t: t) ~depth:_ ~(options: node_js_inspect_options) =
  let major = Basis.inspectWithOptions t.value.at options in
  let minor = options.stylize (Format.sprintf "%f" t.value.sub) "undefined" in
  Format.sprintf "%s|%s" major minor 

let setInspector t = Basis.setInspector t (inspect t)

let add (a: t) =
  match a.next with
    | Some(next) ->
      if a.value.at == next.value.at then
        let sub = (a.value.sub +. next.value.sub) /. 2. in
        let timestamp = { at = a.value.at; sub; isSplicedOut = false } in
        LinkedListImpl.addAfter (!list) a timestamp |> setInspector
      else
        let at = ((a.value.at +. next.value.at) /. 2.) in
        let timestamp = { at; sub = 0.; isSplicedOut = false } in
        LinkedListImpl.addAfter (!list) a timestamp |> setInspector
    | None ->
      let at = if real_time then Js.Date.now () else 0. in
      if at = a.value.at then
        let timestamp = { at; sub = a.value.sub +. 1.; isSplicedOut = false } in
        LinkedListImpl.addToEnd (!list) timestamp |> setInspector
      else
        let timestamp = { at; sub = 0.; isSplicedOut = false } in
        LinkedListImpl.addToEnd (!list) timestamp |> setInspector

let init () =
  list := LinkedListImpl.init ()

let spliceOut (start: t) (stop: t) =
  let rec deleteRange (next: t) =
    if next == stop then ()
    else
      let nextnext = next.next in
      LinkedListImpl.removeNode (!list) next;
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
