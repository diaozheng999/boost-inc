type 'a modref = 'a Modifiable.t
type 'a cc = 'a modref -> Modifiable.changeable

let write x d = Modifiable.write d x

let write' eq x d = Modifiable.write' eq d x

let read r (recv: 'b -> 'a cc) = fun d -> Modifiable.read r (fun x -> recv x d)

let modref = Modifiable.modref

let create = Modifiable.create

let memoize pad key f =
  let run_memoized f r =
    let t1 = !(Modifiable.latest) in
    let v = f () in
    let t2 = !(Modifiable.latest) in
    let nt1o = Time.getNext t1 in
    (match nt1o with
      | None -> r := Some(v, None)
      | Some(nt1) ->
        if Time.compare nt1 t2 = CmpImpl.Less then
          r := Some(v, Some(nt1, t2))
        else
          r := Some(v, None));
    v
  in

  let reuse_result (t1, t2) =
    Time.spliceOut (!Modifiable.latest) t1;
    Modifiable.propagateUntil t2
  in

  let memoize' r =
    match !r with
      | None -> run_memoized f r
      | Some(v, t) ->
        match t with
          | None -> v
          | Some(window) -> reuse_result window; v
  in

  memoize' (Memo_table.find pad key (!Modifiable.latest))

let create_pad () = (Memo_table.create (), Memo_table.create ())

let lift (p1, p2) eqb key b f =
  let f' () =
    let r = Modifiable.empty () in
    fun b ->
      Meta.change' eqb r b;
      memoize p2 key (fun _ -> f r)
  in memoize p1 key f' b

let mkLift eqb = lift (create_pad ()) eqb

let mkLift2 eqb eqc key b c f = 
  let staged bval =
    mkLift eqc key c (f bval)
  in
  mkLift eqb key b staged

let mkLiftCC eqb eqd =
  let lifted arg b f =
    let f' b =
      let r = modref (f b) in
      read r (write' eqd)
    in lift (create_pad ()) eqb arg b f'
  in
  lifted

let mkLiftCC2 eqb eqc eqd =
  let lifted arg b c f =
    let f' b c =
      let r = modref (f b c) in
      read r (write' eqd)
    in
    let staged b =
      lift (create_pad ()) eqc arg c (f' b)
    in lift (create_pad ()) eqb arg b staged
  in
  lifted