open Flags
open Observer
open Basis

type time = Time.t
type changeable = unit

let latest = ref (Time.create ()) [@@unboxed]

let finger = ref (!latest) [@@unboxed]

let insertTime () =
  let t = Time.add (!latest) in
  latest := t; t  

type 'a reader = 'a observer
type 'a modval = Empty | Write of 'a * 'a reader list
type 'a modref = 'a modval ref
type 'a t = 'a modref

exception UnsetMod

let advanceTime () = insertTime () |> ignore

let inspectModval v ~depth ~options =
  match v with
    | Empty -> options##stylize "Empty" `undefined
    | Write(v, l) ->
      let vstr = Inspect.withOptions v ~options in
      let lstr = Inspect.list Observer.inspect l ~depth ~options in
      Format.sprintf "{ value=%s, readers=%s }" vstr lstr

let modref f =
  let r = ref Empty in
  f r |> ignore;
  if pretty_output then Inspect.setInspector r (Inspect.ref inspectModval r) else r


let empty () =
  let r = ref Empty in
  if pretty_output then Inspect.setInspector r (Inspect.ref inspectModval r) else r

let create v = 
  let r = ref (Write (v, [])) in
  if pretty_output then Inspect.setInspector r (Inspect.ref inspectModval r) else r

let read modr f =
  match !modr with
    | Empty -> raise UnsetMod
    | Write (v, _) ->
      let t1 = insertTime () in
      let _ = f v in
      let t2 = insertTime () in
      match !modr with
        | Write (v, rs) -> (
          let reader = Observer.makeInc f (t1, t2) in
          let rs' = reader::rs in
          modr := Write (v, rs')
        )
        | _ -> raise UnsetMod


let readAtTime modr r =
  let _ = if Flags.debug_propagate then
    Inspect.log1 "Modifiable.readAtTime: reading %s" r in 
  match !modr with
    | Empty -> raise UnsetMod
    | Write(v, rs) -> Observer.exec (fun rs -> modr := Write(v, rs)) r v rs

let addReadersToQ rs modr =
  Belt.List.forEach rs (Observer.read (readAtTime modr))

let write' comp modr v =
  match !modr with
    | Empty -> modr := Write (v, [])
    | Write (v', rs) ->
      if comp v v' then ()
      else
        let _ = if Flags.debug_propagate then 
          Inspect.log1
            "Modifiable.write': contains %s"
            (Inspect.custom (Inspect.list Observer.inspect) rs)
        in
        (modr := Write (v, []);
        addReadersToQ rs modr)

let change_async comp modr v =
  Js.Promise.make (fun ~resolve ~reject:_ ->
    let resolver = Observer.make
      ~label:"Promise.resolve"
      ~once:true
      (fun _ ->
        let unit = () in
        resolve unit [@bs]) in
    match !modr with
    | Empty ->
        modr := Write (v, []);
        Observer.read (readAtTime modr) resolver  
    | Write (v', rs) ->
        if comp v v' then let unit = () in resolve unit [@bs]
        else (
          modr := Write (v, []);
          Observer.read (readAtTime modr) resolver;
          addReadersToQ rs modr ))  

let write modr v = write' Box.eq modr v

let deref modr =
  match !modr with
    | Empty -> raise UnsetMod
    | Write (v, _) -> v

let deref' modr =
  match !modr with
    | Empty -> None
    | Write (v, _) -> Some v

let propagateUntil endTime =
  let _ = if Flags.debug_propagate then
    Inspect.log1 "propagateUntil %s" endTime
  in
  let rec loop () =
    match Priority_queue.findMin () with
      | None -> ()
      | Some (f, None) -> f (); loop ()
      | Some (f, Some (start, stop)) ->
        if Time.isSplicedOut start then loop ()
        else if Time.compare endTime stop = Less then ()
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
  let _ = if Flags.debug_propagate then
    Js.log "Modifiable.propagate called"
  in
  let rec loop () =
    match Priority_queue.findMin () with
      | None -> ()
      | Some (f, None) ->
        let _ = if Flags.debug_propagate then (
          Inspect.log1 "Modifiable.propagate.loop: executing static observer %s" f
        ) in
        f (); loop ()
      | Some (f, Some (start, stop)) ->
        let _ = if Flags.debug_propagate then (
          Inspect.log2 "Modifiable.propagate.loop: finger %s latest %s" (!finger) (!latest);
          Inspect.log1 "Modifiable.propagate.loop: f %s" f;
          Inspect.log2 "Modifiable.propagate.loop: from %s to %s" start stop
        ) in
        let finger' = !finger in
        latest := start;
        finger := stop;
        f ();
        finger := finger';
        Time.spliceOut (!latest) stop;
        loop ()
  in loop ();
  if Flags.debug_propagate then (
    Inspect.log2 "Modifiable.propagate: loop completed. Finger %s latest %s" (!finger) (!latest);
    Inspect.log1 "Modifiable.propagate: current times: %s" (Time.inspectTime ()) 
  )


let isOutOfFrame start stop =
  not (
    Time.compare (!latest) start = Less &&
    Time.compare stop (!finger) = Less
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

let attachObserver modr ?label f =
  match !modr with
    | Empty -> raise UnsetMod
    | Write (v, rs) ->
      let r = Observer.make ?label f in
      modr := Write (v, r::rs);
      r

let attach_observer_once modr ?label f =
  match !modr with
    | Empty -> raise UnsetMod
    | Write (v, rs) ->
      let r = Observer.make ?label ~once:true f in
      modr := Write (v, r::rs);
      r

let attachObserver1 modr ?label f = attachObserver modr ?label (Obj.magic f)
