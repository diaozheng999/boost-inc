open Boost
open Basis

type propagate_state = {
  mutable latest : Time.t;
  mutable memo : Time.t;
  mutable is_propagating : bool;
}

type task_entry = {
  task : (unit -> unit Js.Promise.t[@bs]);
  window : Time.window option;
}

let state =
  let time = Time.create () in
  { latest = time; memo = time; is_propagating = false }

let lock = Mutex.make ()

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

let execute ~loop =
  let f () =
    state.is_propagating <- true;
    (loop () [@bs])
    |> Bs_interop.exec_unit_finally (fun () -> state.is_propagating <- false)
  in
  Mutex.lock lock f

let until ~time =
  let rec loop =
   fun [@bs] () ->
    match Task_queue.find_min () with
    | None -> Js.Promise.resolve ()
    | Some { task; window = None } ->
        (task () [@bs]) |> Bs_interop.promise_then_0 loop
    | Some { task; window = Some (start, stop) } ->
        if Time.isSplicedOut start then loop () [@bs]
        else if Time.compare time stop = Less then Js.Promise.resolve ()
        else
          let finger = state.memo in
          state.latest <- start;
          state.memo <- stop;
          (task () [@bs])
          |> Js.Promise.then_ (fun () ->
                 state.memo <- finger;
                 Time.spliceOut state.latest stop;
                 loop () [@bs])
  in
  execute ~loop

let exec () =
  let rec loop =
   fun [@bs] () ->
    match Task_queue.find_min () with
    | None -> Js.Promise.resolve ()
    | Some { task; window = None } ->
        (task () [@bs]) |> Bs_interop.promise_then_0 loop
    | Some { task; window = Some (start, stop) } ->
        if Time.isSplicedOut start then loop () [@bs]
        else
          let finger = state.memo in
          state.latest <- start;
          state.memo <- stop;
          (task () [@bs])
          |> Js.Promise.then_ (fun () ->
                 state.memo <- finger;
                 Time.spliceOut state.latest stop;
                 loop () [@bs])
  in
  execute ~loop

let when_not_propagating f =
  if state.is_propagating then
    Mutex.acquire_readonly_access lock
    |> Bs_interop.unstable_promise_then_unit_exec f
  else Js.Promise.resolve ((Bs_interop.arity0 f) () [@bs])
