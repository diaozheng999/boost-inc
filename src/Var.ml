open Basis
open Combinators

type 'a var = {
  modref : 'a Box.t modref;
  change : 'a -> unit;
  create : 'a -> 'a Box.t;
  eq : 'a Box.t equality;
  change_eagerly : 'a -> unit;
  deref : unit -> 'a;
  subscribe : ('a -> unit) -> unit -> unit;
  subscribe_box : ('a Box.t -> unit) -> unit -> unit;
  subscribe_uncurried : ((('a -> unit)[@bs]) -> (unit -> unit[@bs])[@bs]);
  subscribe_box_uncurried :
    ((('a Box.t -> unit)[@bs]) -> (unit -> unit[@bs])[@bs]);
  change_to : ?exec_after:('a -> unit) -> ('a -> 'a) -> unit;
  change_to_eagerly : ?exec_after:('a -> unit) -> ('a -> 'a) -> unit;
  changeEagerly : 'a -> unit; [@deprecated]
  subscribeBox : ('a Box.t -> unit) -> unit -> unit; [@deprecated]
  subscribe1 : ((('a -> unit)[@bs]) -> (unit -> unit[@bs])[@bs]); [@deprecated]
  subscribeBox1 : ((('a Box.t -> unit)[@bs]) -> (unit -> unit[@bs])[@bs]);
      [@deprecated]
}

type 'a t = 'a var

let change ({ modref; create; eq } : 'a var) a =
  let next = create a in
  Modifiable.change' eq modref next

let change_to ({ modref; create; eq } : 'a var) ?exec_after change =
  Combinators.write_to
    ( modref >>= fun v ->
      let v = Box.value v in
      let next = change v in
      let _ = match exec_after with Some exe -> exe next | None -> () in
      write' eq (create next) )
    modref

let change_to_eagerly var ?exec_after change =
  change_to var ?exec_after change;
  Modifiable.propagate ()

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
  fun [@bs] f -> (staged (fun [@bs] v -> (f (Box.valueOf v) [@bs])) [@bs])

let createVarFromModref ?(eq = Box.eq) ?label create modref =
  let subscribe_box = subscribeBox ?label modref in
  let subscribe_box_uncurried = subscribeBox1 ?label modref in
  let subscribe = subscribe ?label modref in
  let subscribe_uncurried = subscribe1 ?label modref in
  let deref () = Modifiable.deref modref |> Box.valueOf in
  let rec v =
    {
      modref;
      change = (fun a -> change v a);
      change_eagerly = (fun a -> changeEagerly v a);
      create;
      eq;
      subscribe;
      subscribe_uncurried;
      subscribe_box;
      subscribe_box_uncurried;
      changeEagerly = (fun a -> changeEagerly v a);
      subscribe1 = subscribe_uncurried;
      subscribeBox = subscribe_box;
      subscribeBox1 = subscribe_box_uncurried;
      deref;
      change_to = (fun ?exec_after f -> change_to v ?exec_after f);
      change_to_eagerly =
        (fun ?exec_after f -> change_to_eagerly v ?exec_after f);
    }
  in
  v

let createVar ?(eq = Box.eq) create v =
  let modref = Modifiable.create (create v) in
  createVarFromModref ~eq create modref

let create ?(label = "var") = createVar (Box.create ~label)

let empty ?(label = "loc") () =
  let modref = Modifiable.empty () in
  createVarFromModref ~eq:Box.eq (Box.create ~label) modref

let int = createVar Box.fromInt

let opt o = createVar Box.fromOption o

let str s = createVar Box.fromString s

let withCustomHashFunction ~hash v =
  createVar (Box.withCustomHashFunction ~hash) v

let createAssumingSameType ?(label = "infer") v =
  match Js.Types.classify v with
  | Js.Types.JSString s -> str s |> Obj.magic
  | Js.Types.JSNull | Js.Types.JSNumber _ | Js.Types.JSSymbol _
  | Js.Types.JSTrue | Js.Types.JSFalse ->
      createVar (Box.fromPrim "%prim") v
  | _ -> create ~label v

let ofCombinator (r : 'a cc) =
  let inst = modref r in
  createVarFromModref ~eq:Box.eq (Box.create ~label:"cc") inst

let observe ~f (v : 'a var) = Modifiable.observe v.modref f

let log ?l v =
  let f v =
    match l with
    | Some label -> Js.Console.log2 label v
    | None -> Js.Console.log v
  in
  observe ~f v

let ( >>> ) v recv = v.modref >>= fun value -> Box.valueOf value |> recv

let of_modref = createVarFromModref

let of_combinator = ofCombinator

let make_with_custom_hash_function = withCustomHashFunction

let make = create

let make_var = createVar

let change_eagerly = changeEagerly

let make_assuming_same_type = createAssumingSameType

let make_custom ~hash ~equal v =
  make_var ~eq:(Box.mk_eq equal) (Box.with_custom_hash_function ~hash) v
