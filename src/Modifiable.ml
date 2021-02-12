open Basis
open Flags

type time = Time.t
type changeable = unit

let latest = ref (Time.create ()) [@@unboxed]

let finger = ref (!latest) [@@unboxed]

let insertTime () =
  let t = Time.add (!latest) in
  latest := t; t

type 'a reader = ('a -> unit) * Time.window
type 'a modval = Empty | Write of 'a * 'a reader list
type 'a modref = 'a modval ref
type 'a t = 'a modref

exception UnsetMod


let inspectReader (_, (f, t)) ~depth ~options =
  if depth < 0 then options.stylize "[reader]" "special" else
    let f = inspectWithOptions f options in
    let t = inspectWithOptions t options in
    Format.sprintf "reader from %s to %s" f t

let inspectModval v ~depth ~options =
  match v with
    | Empty -> options.stylize "Empty" "undefined"
    | Write(v, l) ->
      let vstr = inspectWithOptions v options in
      let lstr = inspectList inspectReader l ~depth ~options in
      Format.sprintf "{ value=%s, readers=%s }" vstr lstr

let modref f =
  let r = ref Empty in
  f r |> ignore;
  if pretty_output then setInspector r (inspectRef inspectModval r) else r


let empty () =
  let r = ref Empty in
  if pretty_output then setInspector r (inspectRef inspectModval r) else r

let create v = 
  let r = ref (Write (v, [])) in
  if pretty_output then setInspector r (inspectRef inspectModval r) else r

let read modr f =
  match !modr with
    | Empty -> raise UnsetMod
    | Write(v, _) ->
      let t1 = insertTime () in
      let _ = f v in
      let t2 = insertTime () in
      match !modr with
        | Write (v, rs) -> (
          let rs' = (f, (t1, t2))::rs in
          modr := Write(v, rs')
        )
        | _ -> raise UnsetMod

let readAtTime modr ((f, _) as r) =
  match !modr with
    | Empty -> raise UnsetMod
    | Write(v, rs) -> (modr := Write(v, r::rs); f v)

let addReadersToQ rs modr =
  let addReader ((_, (t1, t2)) as r) =
    if Time.isSplicedOut t1 then ()
    else Priority_queue.insert ((fun () -> readAtTime modr r), (t1, t2))
  in
  Belt.List.forEach rs addReader

let write' comp modr v =
  match !modr with
    | Empty -> modr := Write (v, [])
    | Write (v', rs) ->
      if comp v v' then ()
      else
        (modr := Write (v, []);
        addReadersToQ rs modr)

let write modr v = write' Box.eq modr v

let deref modr =
  match !modr with
    | Empty -> raise UnsetMod
    | Write (v, _) -> v

let propagateUntil endTime =
  let rec loop () =
    match Priority_queue.findMin () with
      | None -> ()
      | Some(f, (start, stop)) ->
        if Time.isSplicedOut start then loop ()
        else if Time.compare endTime stop = CmpImpl.Less then ()
        else
          let finger' = !finger in
          latest := start;
          finger := stop;
          f () ;
          finger := finger';
          Time.spliceOut (!latest) stop;
          loop ()
  in loop ()

let propagate () =
  let rec loop () =
    match Priority_queue.findMin () with
      | None -> ()
      | Some(f, (start, stop)) ->
        let finger' = !finger in
        latest := start;
        finger := stop;
        f ();
        finger := finger';
        Time.spliceOut (!latest) stop;
        loop ()
  in loop ()

let isOutOfFrame start stop =
  not (
    Time.compare (!latest) start = CmpImpl.Less &&
    Time.compare stop (!finger) = CmpImpl.Less
  )

let init () =
  Time.init ();
  latest := Time.create ();
  Priority_queue.init ()

let change l v = write' Box.eq l v

let change' comp l v = write' comp l v

let never _ _ = false

let change'' l v = write' never l v

let observe modr f =
    match !modr with
      | Empty -> raise UnsetMod
      | Write(v, _) -> f v
