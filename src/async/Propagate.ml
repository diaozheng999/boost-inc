open Boost
open Basis

type propagate_state = {
  mutable latest : Time.t;
  mutable memo : Time.t;
  mutable is_propagating : bool;
  mutable propagation_began_at : float;
}

type task_entry = {
  task : (unit -> unit Js.Promise.t[@bs]);
  window : Time.window option;
}

type propagate_owners = [ `Variable_update | `Propagate ]

type 'a inst = [ `Read of 'a | `Write of 'a ]

let state =
  let time = Time.create () in
  {
    latest = time;
    memo = time;
    is_propagating = false;
    propagation_began_at = 0.;
  }

let mutex : propagate_owners Asym_lock.t = Asym_lock.make ~mutex:(Mutex.make ())

let change_gen = Unique.make_with_label ~label:"Propagate.change"

module Task_queue = struct
  type t = task_entry

  let compare { window = sa; _ } { window = sb; _ } =
    match (sa, sb) with
    | Some a, Some b -> Time.compareWindow a b
    | None, Some _ -> Less
    | Some _, None -> Greater
    | None, None -> Equal

  let queue : (t, t) Heap.t = Heap.make (Compare.abs compare)

  let is_valid { window; _ } =
    match window with
    | Some (start, _) -> not (Time.isSplicedOut start)
    | _ -> true

  let insert ?window task =
    let p = { window; task } in
    Heap.push queue p ~p

  let rec find_min () =
    match Heap.pop queue with
    | None -> None
    | Some value -> if is_valid value then Some value else find_min ()
end

let insert_time () =
  let t = Time.add state.latest in
  state.latest <- t;
  t

let execute ~loop =
  let f =
   fun [@bs] () ->
    state.propagation_began_at <- Js.Date.now ();
    state.is_propagating <- true;
    (loop 0 [@bs])
    |> Bs_interop.exec_unit_finally (fun n ->
           state.is_propagating <- false;
           Js.log "Propagation complete.";
           Printf.ksprintf Js.log "  Executed %d task(s)." n;
           Printf.ksprintf Js.log "  Took %f s."
             ((Js.Date.now () -. state.propagation_began_at) /. 1000.))
  in
  Asym_lock.acquire ~mutex ~owner:`Propagate f

let until ~time =
  let rec loop =
   fun [@bs] n ->
    match Task_queue.find_min () with
    | None -> Js.Promise.resolve n
    | Some { task; window = None } ->
        (task () [@bs]) |> Js.Promise.then_ (fun () -> (loop (n + 1) [@bs]))
    | Some { task; window = Some (start, stop) } ->
        if Time.isSplicedOut start then loop n [@bs]
        else if Time.compare time stop = Less then Js.Promise.resolve n
        else
          let finger = state.memo in
          state.latest <- start;
          state.memo <- stop;
          (task () [@bs])
          |> Js.Promise.then_ (fun () ->
                 state.memo <- finger;
                 Time.spliceOut state.latest stop;
                 loop (n + 1) [@bs])
  in
  execute ~loop

let exec () =
  let rec loop =
   fun [@bs] n ->
    match Task_queue.find_min () with
    | None -> Js.Promise.resolve n
    | Some { task; window = None } ->
        (task () [@bs]) |> Js.Promise.then_ (fun () -> (loop (n + 1) [@bs]))
    | Some { task; window = Some (start, stop) } ->
        if Time.isSplicedOut start then loop n [@bs]
        else
          let finger = state.memo in
          state.latest <- start;
          state.memo <- stop;
          (task () [@bs])
          |> Js.Promise.then_ (fun () ->
                 state.memo <- finger;
                 Time.spliceOut state.latest stop;
                 loop (n + 1) [@bs])
  in
  execute ~loop

let when_not_propagating f = Asym_lock.acquire ~mutex ~owner:`Variable_update f
