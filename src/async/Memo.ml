open Boost

type ('a, 'b) t = {
  next : (('a -> 'b Js.Promise.t)[@bs]) Memo_table.t;
  result : 'b Memo_table.t;
}

type ('a, 'b) bind_t = { arg : 'a Memo_table.t; result : 'b Memo_table.t }


let memoize pad key ~next =
  let run_memoized f r =
    let t1 = Propagate.state.latest in
    f ()
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
  Propagate.when_not_propagating memoize'

let create_memo ~name () =
  let next = Memo_table.create ~name:(name ^ "$next") () in
  let result = Memo_table.create ~name:(name ^ "$result") () in
  { next; result }

let lift memo key f =
  memoize memo.next key ~next:(fun () ->
      let result = Variable.make_intermediate () in
      let resolved =
       fun [@bs] b ->
        Variable.write result b |> Js.Promise.then_ (fun () -> f result)
      in
      Js.Promise.resolve resolved)

let mk_map f =
  let memo = Memo_table.create ~name:"mk_map$memo" () in
  fun v ->
    let result = Variable.make_intermediate () in
    Variable.read_with_address v
    |> Js.Promise.then_ (fun (value, key) ->
           memoize memo (Address.as_uniq key) ~next:(fun () ->
               f value
               |> Js.Promise.then_ (Variable.write result)
               |> Js.Promise.then_ (fun () -> Js.Promise.resolve result)))
