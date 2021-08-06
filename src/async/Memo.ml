open Boost

type 'a changeable = 'a Variable.t -> unit Js.Promise.t

type ('a, 'b) t = {
  next : (('a -> 'b Js.Promise.t)[@bs]) Memo_table.t;
  result : 'b Memo_table.t;
}

type ('a, 'b) bind_t = { arg : 'a Memo_table.t; result : 'b Memo_table.t }

external ( >>= ) : 'a Js.Promise.t -> ('a -> 'b Js.Promise.t) -> 'b Js.Promise.t
  = "then"
  [@@bs.send]

external ( >>| ) : 'a Js.Promise.t -> ('a -> 'b) -> 'b Js.Promise.t = "then"
  [@@bs.send]

let mk_lift_cc_memo_gen = Unique.make_with_label ~label:"Memo$mk_lift_cc$copy"

let mk_lift_cc_gen = Unique.make_with_label ~label:"Memo$mk_lift_cc"

let mk_map_gen = Unique.make_with_label ~label:"Memo$mk_map"

let memoize pad key ~(next : 'a Async_callback.t) =
  let run_memoized f r =
    let t1 = Propagate.state.latest in
    Async_callback.act f
    |> Bs_interop.unstable_promise_then_unit_exec (fun v ->
           let t2 = Propagate.state.latest in
           let () =
             match Time.getNext t1 with
             | Some nt1 when Time.compare nt1 t2 = Less ->
                 r := Some (v, Some (nt1, t2))
             | _ -> r := Some (v, None)
           in
           Memo_table.set pad key r;
           v)
  in
  let reuse_result (t1, t2) =
    Time.spliceOut Propagate.state.latest t1;
    Propagate.until ~time:t2
  in
  let memoize' =
   fun [@bs] () ->
    let result = Memo_table.find pad key Propagate.state.latest in
    match !result with
    | None -> run_memoized next result
    | Some (v, t) -> (
        match t with
        | None -> Js.Promise.resolve v
        | Some window ->
            reuse_result window
            |> Bs_interop.unstable_promise_then_unit_exec (fun () -> v))
  in
  (memoize' () [@bs])

let create_memo ~name =
  let arg = Memo_table.create ~name:(name ^ "$arg") () in
  let result = Memo_table.create ~name:(name ^ "$result") () in
  { arg; result }

(* ('a, 'b) t -> 'a Variable.t -> ('a Variable.t -> 'b Async_callback.t) -> 'b Js.Promise.t *)
let lift memo variable f =
  let key = Variable.address_of variable in

  let promise ~resolve ~reject:_ =
    let (_ : _ Js.Promise.t) =
      memoize memo.arg key ~next:(fun [@bs] next ->
          let result = Variable.make_intermediate () in
          let resolved b =
            Variable.write result b >>= fun () ->
            memoize memo.result key ~next:(f result)
          in
          (next resolved [@bs]))
      >>= fun memoized ->
      Variable.read variable (fun value ->
          memoized value
          |> Bs_interop.unstable_promise_then_unit_exec (fun result ->
                 (resolve result [@bs])))
    in
    ()
  in
  Js.Promise.make promise

let mk_lift ~name = lift (create_memo ~name)

let mk_lift_cc ?name ?stack () =
  let name =
    match name with Some name -> name | None -> Unique.string mk_lift_cc_gen
  in

  let memo = create_memo ~name in
  let stack = Changeable.Stack.push stack (Unique.of_str name) in
  let lifted variable f =
    let change result =
      let partial_applied dest =
        let changeable = f result in
        let _ = Variable.describe result in
        let _ = Js.log (Changeable.to_string changeable) in
        Changeable.log_execution_context ();
        Changeable.variable changeable >>= fun src ->
        Variable.copy ~label:(Unique.string mk_lift_cc_memo_gen) ~src dest
      in
      fun [@bs] next ->
        Js.log "mk_lift_cc.change";
        next partial_applied [@bs]
    in
    lift memo variable change
  in
  fun variable f ->
    Changeable.make ~label:(Unique.of_str "mk_lift_cc")
      ~stack ~kind:"mk_lift_cc" (fun [@bs] dest ->
        lifted variable f >>= fun exec -> exec dest)

let mk_map ?label ?stack a f =
  let gen = Belt.Option.getWithDefault label mk_map_gen in
  let name = Unique.string gen in
  let stack = Changeable.Stack.push stack (Unique.of_str name) in

  let lift = mk_lift ~name in

  let mapped aref =
    let changeable =
      Changeable.read ~stack aref (fun aval ->
          Changeable.make ~stack ~kind:"mk_map/map_and_write_result"
            (fun [@bs] dest -> f aval >>= Variable.write dest))
    in
    Async_callback.return changeable
  in
  lift a mapped

let map a f =
  let stack =
    Changeable.Stack.push None
      (Unique.value (Unique.make_with_label ~label:"map"))
  in

  let mapper b =
    let bref = Variable_prim.mem b in
    mk_map ~stack a f
    |> Js.Promise.then_ (fun changeable -> Changeable.act changeable bref)
    |> Bs_interop.unstable_promise_then_unit_exec (fun () -> bref)
  in
  Variable.get_value a >>= f >>= mapper
