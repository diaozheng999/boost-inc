open Boost

exception UnsubIncReader

type 'a observer =
  | Observe of {
      loc: Unique.t;
      read: 'a -> unit;
      once: bool;
      mutable isActive: bool;
    }
  | Inc of {
      loc: Unique.t;
      read: 'a -> unit;
      window: Time.window;
    }

type 'a t = 'a observer

let unsub obs =
  match obs with
    | Inc _ -> raise UnsubIncReader
    | Observe obs ->
        obs.isActive <- false

let defaultGen = Unique.make_with_label ~label:"obs"

let inspect v ~depth ~options =
  if depth < 0 then options##stylize "[reader]" `special else
    match v with
      | Observe { loc; isActive = false } ->
          let s = Format.sprintf
            "{ Observer %s [inactive] }"
            (Unique.to_str loc)
          in options##stylize s `undefined
      | Inc { window = (t1, t2); loc } ->
          let t1 = Inspect.time t1 ~depth ~options in
          let t2 = Inspect.time t2 ~depth ~options in
          Format.sprintf "{ Inc_reader %s from %s to %s }"
            (options##stylize (Unique.to_str loc) `string)
            t1
            t2
      | Observe { loc } ->
          Format.sprintf "{ Observer %s }"
            (options##stylize (Unique.to_str loc) `string)

let make ?label ?once read =
  let gen = match label with
    | Some (label) -> Unique.make_with_label ~label
    | None -> defaultGen
  in
  let loc = Unique.value gen in
  let once = Belt.Option.getWithDefault once false in
  let obs = Observe { read; loc; isActive = true; once } in
  if Flags.pretty_output then Inspect.setInspector obs (inspect obs) else obs

let makeInc read window =
  let loc = Unique.value defaultGen in
  let obs = Inc { read; window; loc } in
  if Flags.pretty_output then Inspect.setInspector obs (inspect obs) else obs

let read f reader =
  match reader with
    | Inc { window = (t1, _) } when Time.isSplicedOut t1 -> ()
    | Observe { isActive = false } -> ()
    | Inc { window } ->
        Priority_queue.insert ((fun () -> f reader), Some window)
    | Observe { once = false } ->
        Priority_queue.insert ((fun () -> f reader), None)
    | Observe observer ->
        Priority_queue.insert (
          (fun () -> f reader; observer.isActive <- false),
          None)

let exec f reader v rs =
  match reader with
    | Observe { isActive = false } -> f rs
    | Observe { read }
    | Inc { read } -> f (reader::rs); read v
