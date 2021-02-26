open Boost

exception UnsubTimedObserver

type 'a observer =
  | Timed of {
      read: 'a -> unit;
      window: Time.window;
      loc: Unique.t;
    }
  | Observe of {
      read: 'a -> unit;
      loc: Unique.t;
      mutable isActive: bool
    }

type 'a t = 'a observer

let unsub obs =
  match obs with
    | Observe obs -> obs.isActive <- false
    | _ -> raise UnsubTimedObserver

let defaultGen = Unique.makeWithLabel ~label:"obs"

let make ?label ?window read =
  let gen = match label with
    | Some (label) -> Unique.makeWithLabel ~label
    | None -> defaultGen
  in
  let loc = Unique.value gen in
  match window with
    | Some window -> Timed { read; window; loc }
    | _ -> Observe { read; loc; isActive = true }

let makeInc read window =
  let loc = Unique.value defaultGen in
  Timed { read; window; loc }
