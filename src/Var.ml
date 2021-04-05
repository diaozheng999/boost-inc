open Basis
open Combinators

type 'a var = {
  modref: 'a Box.t modref;
  change: 'a -> unit;
  create: 'a -> 'a Box.t;
  eq: 'a Box.t equality;
  changeEagerly: 'a -> unit;
  deref: unit -> 'a;
  subscribe: ('a -> unit) -> unit -> unit;
  subscribeBox: ('a Box.t -> unit) -> unit -> unit;
  subscribe1: (('a -> unit [@bs]) -> (unit -> unit [@bs]) [@bs]);
  subscribeBox1: (('a Box.t -> unit [@bs]) -> (unit -> unit [@bs]) [@bs]);
}

type 'a t = 'a var

let change ({ modref; create; eq }: 'a var) a =
  let next = create a in
  Modifiable.change' eq modref next 

let changeEagerly v a =
  change v a;
  Modifiable.propagate ()

let subscribeBox ?label modref f =
  let observer = Modifiable.attachObserver modref ?label f in
  fun () -> Observer.unsub observer

let subscribeBox1 ?label modref =
  fun [@bs] f ->
    let observer = Modifiable.attachObserver1 modref ?label f in
    fun [@bs] () -> Observer.unsub observer

let subscribe ?label modref f =
  subscribeBox ?label modref (fun v -> f (Box.valueOf v))

let subscribe1 ?label modref =
  let staged = subscribeBox1 ?label modref in
  fun [@bs] f -> staged (fun [@bs] v -> f (Box.valueOf v) [@bs]) [@bs]

let createVarFromModref ?(eq=Box.eq) ?label create modref =
  let subscribeBox = subscribeBox ?label modref in
  let subscribeBox1 = subscribeBox1 ?label modref in
  let subscribe = subscribe ?label modref in
  let subscribe1 = subscribe1 ?label modref in
  let deref () = Modifiable.deref modref |> Box.valueOf in
  let rec v = {
    modref;
    change = (fun a -> change v a);
    changeEagerly = (fun a -> changeEagerly v a);
    create;
    eq;
    subscribe;
    subscribe1;
    subscribeBox;
    subscribeBox1;
    deref;
  } in v

let createVar ?(eq=Box.eq) create v =
  let modref = Modifiable.create (create v) in
  createVarFromModref ~eq create modref

let create ?(label="var") = createVar (Box.create ~label)

let empty ?(label="loc") () =
  let modref = Modifiable.empty () in
  createVarFromModref ~eq:Box.eq (Box.create ~label) modref

let int = createVar Box.fromInt

let opt o = createVar Box.fromOption o

let str s = createVar Box.fromString s

let withCustomHashFunction ~hash v =
  createVar (Box.withCustomHashFunction ~hash) v

let createAssumingSameType ?(label="infer") v =
  match Js.Types.classify v with
    | Js.Types.JSString s -> str s |> Obj.magic
    | Js.Types.JSNull
    | Js.Types.JSNumber _
    | Js.Types.JSSymbol _
    | Js.Types.JSTrue
    | Js.Types.JSFalse -> createVar (Box.fromPrim "%prim") v
    | _ -> create ~label v

let ofCombinator (r: 'a cc) =
  let inst = modref r in
  createVarFromModref ~eq:Box.eq (Box.create ~label:"cc") inst

let observe ~f (v: 'a var) = Modifiable.observe v.modref f

let log ?l v =
  let f v =
    match l with
      | Some label -> Js.Console.log2 label v
      | None -> Js.Console.log v
  in
  observe ~f v

let (>>>) v recv = v.modref >>= fun value -> Box.valueOf value |> recv
