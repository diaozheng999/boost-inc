type 'a t = ((('a -> unit)[@bs]) -> unit[@bs])

type ('a, 'b) response = {
  mutable a_value : 'a option;
  mutable b_value : 'b option;
}

let return p = fun [@bs] next -> (next p [@bs])

let map_uncurried v ~f =
 fun [@bs] next ->
  let mapped = fun [@bs] a -> (next (f a [@bs]) [@bs]) in
  (v mapped [@bs])

let map v ~f = map_uncurried v ~f:(fun [@bs] x -> f x)

let both (a : 'a t) (b : 'b t) : ('a * 'b) t =
  let state = { a_value = None; b_value = None } in
  fun [@bs] next ->
    let proceed () =
      match (state.a_value, state.b_value) with
      | Some a, Some b -> next (a, b) [@bs]
      | _ -> ()
    in
    a (fun [@bs] res ->
        state.a_value <- Some res;
        proceed ()) [@bs];
    b (fun [@bs] res ->
        state.b_value <- Some res;
        proceed ()) [@bs]

let act (f : 'a t) =
  let promise ~resolve ~reject:_ = (f resolve [@bs]) in
  Js.Promise.make promise

let apply f a =
 fun [@bs] b_next ->
  (f (fun [@bs] f' -> (a (fun [@bs] a_val -> (b_next (f' a_val) [@bs])) [@bs]))
   [@bs])

let bind a ~f =
 fun [@bs] b_next -> (a (fun [@bs] a_val -> ((f a_val) b_next [@bs])) [@bs])

let ( <*> ) = apply

let ( >>| ) a f = map a ~f

let ( >>= ) a f = bind a ~f

let ( let* ) a f = bind a ~f

let ( let+ ) a f = map a ~f

let ( and+ ) = both

module P = struct
  let return v =
   fun [@bs] next ->
    let (_ : _ Js.Promise.t) =
      Bs_interop.unstable_promise_then_unit_exec
        (fun result -> (next result [@bs]))
        v
    in
    ()

  let map a ~f = bind a ~f:(fun v -> return (f v))

  let both = both

  let bind a ~f =
   fun [@bs] next ->
    let exec_promise mapped =
      mapped next [@bs];
      Js.Promise.resolve ()
    in
    let bound_a =
     fun [@bs] res ->
      let _ = f res |> Js.Promise.then_ exec_promise in
      ()
    in
    (a bound_a [@bs])

  let apply f a =
   fun [@bs] b_next ->
    (f (fun [@bs] map ->
         (a (fun [@bs] a_val ->
              let (_ : _ Js.Promise.t) =
                map a_val
                |> Bs_interop.unstable_promise_then_unit_exec (fun b ->
                       (b_next b [@bs]))
              in
              ()) [@bs])) [@bs])
    
  let ( <*> ) = apply
  let ( >>| ) a f = map a ~f
  let ( >>= ) a f = bind a ~f
  let ( let* ) a f = bind a ~f
  let ( let+ ) a f = map a ~f
  let ( and+ ) = both
end
