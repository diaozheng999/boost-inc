open Boost

type ('k, 'v) js_map

type 'a table = (Address.t, ('a * Time.window option) Linked_list.t) js_map



let memoize pad key ~next =
  let run_memoized f r =
    let t1 = Propagate.state.latest in
    f ()
    |> Bs_interop.unstable_promise_then_unit_exec (fun v ->
           let t2 = Propagate.state.latest in
           let () = match Time.getNext t1 with
            | Some nt1 when Time.compare nt1 t2 = Less -> r := Some (v, Some (nt1, t2))
            | _ -> r := Some (v, None)
           in
           Memo_table.set pad key r;
           v)
  in
  failwith "Not yet implemented"