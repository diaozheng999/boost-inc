open Infix

type time = Time.t
type changeable = unit Js.Promise.t

let latest = ref (Time.create ())

let finger = ref (!latest)

let insertTime () =
  let t = Time.add (!latest) in
  latest := t; t

type 'a reader = ('a -> unit Js.Promise.t) * time * time
type 'a modval = Empty | Write of 'a * 'a reader list
type 'a modref = 'a modval ref
type 'a t = 'a modref

exception UnsetMod

let resolve = Js.Promise.resolve

let empty () = ref Empty
let create v = ref (Write (v, []))
let modref f =
  let r = ref Empty in
  f r |> ignore; r

let read modr f =
  match !modr with
    | Empty -> raise UnsetMod
    | Write(v, _) ->
      let t1 = insertTime () in
      f v >>| fun () ->
        let t2 = insertTime () in
        match !modr with
          | Write (v, rs) -> (
            let rs' = (f, t1, t2)::rs in
            modr := Write(v, rs')
          )
          | _ -> raise UnsetMod

let readAtTime modr ((f, _, _) as r) =
  match !modr with
    | Empty -> raise UnsetMod
    | Write(v, rs) -> (modr := Write(v, r::rs); f v)

let addReadersToQ rs modr =
  let addReader ((_, t1, t2) as r) =
    if Time.isSplicedOut t1 then ()
    else Priority_queue.insert ((fun () -> readAtTime modr r), t1, t2)
  in
  Belt.List.forEach rs addReader

let writeAtom comp modr v =
  match !modr with
    | Empty -> modr := Write (v, [])
    | Write (v', rs) ->
      if comp v v' then ()
      else
        (modr := Write (v, []);
        addReadersToQ rs modr)

let write' comp modr v = resolve (writeAtom comp modr v)

let write modr v = write' Box.eq modr v

let deref modr =
  match !modr with
    | Empty -> raise UnsetMod
    | Write (v, _) -> v

let propagateUntil endTime =
  let rec loop () =
    match Priority_queue.findMin () with
      | None -> resolve ()
      | Some(f, start, stop) ->
        if Time.isSplicedOut start then loop ()
        else if Time.compare endTime stop = CmpImpl.Less then resolve ()
        else
          let finger' = !finger in
          latest := start;
          finger := stop;
          f () >>= fun () ->
            finger := finger';
            Time.spliceOut (!latest) stop;
            loop ()
  in loop ()

let propagate () =
  let rec loop () =
    match Priority_queue.findMin () with
      | None -> resolve ()
      | Some(f, start, stop) ->
        let finger' = !finger in
        latest := start;
        finger := stop;
        f () >>= fun () ->
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

let change l v = writeAtom Box.eq l v

let change' comp l v = writeAtom comp l v

let never _ _ = false

let change'' l v = writeAtom never l v
