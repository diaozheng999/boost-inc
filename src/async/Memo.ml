let memoize pad key ~next =
  let run_memoized f r =
    let t1 = Propagate.state.latest in
    f () |> Js.Promise.then_ (fun v ->
      let t2 = Propagate.state.latest in
      let nt1o = Time.getNext t1 in
      failwith "Not yet implemented"
    )
  in
  failwith "Not yet implemented"