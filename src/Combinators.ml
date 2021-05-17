open Basis

type 'a modref = 'a Modifiable.t

type 'a cc = 'a modref -> Modifiable.changeable

type ('b, 'd) pad = 'b Memo_table.t * 'd Memo_table.t

let debug = Flags.debug_combinators

let write x d = Modifiable.write d x

let write' eq x d = Modifiable.write' eq d x

let read r (recv : 'b -> 'a cc) d = Modifiable.read r (fun x -> recv x d)

let modref = Modifiable.modref

let create = Modifiable.create

let memoize pad key f =
  let _ = if debug then Js.Console.log2 "memoize: called on pad" pad else () in
  let run_memoized f r =
    let _ =
      if debug then
        Inspect.log2 "memoize.run_memoized: called with entry %s pad %s" r pad
      else ()
    in
    let t1 = !Modifiable.latest in
    let v = f () in
    let t2 = !Modifiable.latest in
    let nt1o = Time.getNext t1 in
    if debug then
      Js.log "memoize.run_memoized: executed f with following results:";
    if debug then Inspect.log1 "    t1 %s" t1;
    if debug then Inspect.log1 "    t2 %s" t2;
    if debug then Inspect.log1 "  nt1o %s" nt1o;
    (match nt1o with
    | None -> r := Some (v, None)
    | Some nt1 ->
        if Time.compare nt1 t2 = Less then r := Some (v, Some (nt1, t2))
        else r := Some (v, None));
    if debug then Inspect.log1 "memoize.run_memoized: updated modref %s" r;
    Memo_table.set pad key r;
    if debug then
      Inspect.log1 "memoize.run_memoized: updated memotable, new table %s" pad;
    v
  in

  let reuse_result (t1, t2) =
    if debug then Inspect.log2 "memoize.reuse_result: splicing range %s - %s" t1 t2;
    Time.spliceOut !Modifiable.latest t1;
    Modifiable.propagateUntil t2
  in

  let memoize' r =
    if debug then Inspect.log1 "memoize.memoize': called with %s" r;
    match !r with
    | None ->
        let _ =
          if debug then Js.log "memoize.memoize': not found. Executing f"
        in
        run_memoized f r
    | Some (v, t) -> (
        let _ =
          if debug then Js.log "memoize.memoize': reusing existing value"
        in
        match t with
        | None -> v
        | Some window ->
            reuse_result window;
            v)
  in
  memoize' (Memo_table.find pad key !Modifiable.latest)

let create_pad ?name () =
  match (debug, name) with
  | true, Some name ->
      ( Memo_table.create ~name:(name ^ "$input") (),
        Memo_table.create ~name:(name ^ "$output") () )
  | true, _ ->
      let uniq = Box.getLabel ~label:"Pad" () |> Box.uniq_to_string in
      ( Memo_table.create ~name:(uniq ^ "$input") (),
        Memo_table.create ~name:(uniq ^ "$output") () )
  | _ -> (Memo_table.create (), Memo_table.create ())

let lift (p1, p2) eqb key b f =
  let _ = if debug then Inspect.log2 "lift: called with key %s value %s" key b else () in
  let _ =
    if debug then
      Inspect.log2 "lift: current time %s; current finger %s" (!Modifiable.latest) 
        (!Modifiable.finger)
  in
  let f' () =
    let _ = if debug then Js.log "lift.f': called" in
    let r = Modifiable.empty () in
    fun b ->
      Meta_.change' eqb r b;
      memoize p2 key (fun _ -> f r)
  in
  memoize p1 key f' b

let mk_lift eqb = lift (create_pad ()) eqb

let mk_lift_cc ?fname eqb eqd =
  let _ =
    if debug then Inspect.log1 "mk_lift_cc: created lifter for %s" fname else ()
  in
  let pad = create_pad ?name:fname () in
  let lifted arg b f =
    let _ =
      if debug then Inspect.log2 "mk_lift_cc.lifted: called with %s %s" arg b else ()
    in
    let _ =
      if debug then Inspect.log1 "mk_lift_cc.lifted: current time %s" !Modifiable.latest
    in
    let f' b =
      let _ =
        if debug then Inspect.log2 "mk_lift_cc.lifted.f': called with %s %s" b arg
      in
      let r = modref (f b) in
      read r (write' eqd)
    in
    lift pad eqb arg b f'
  in
  lifted

let ( >>= ) = read

let log modr = Modifiable.observe modr Js.log

let write_to f r = f r |> ignore

(** deprecated *)
let mkLift = mk_lift

let mkLiftCC = mk_lift_cc
