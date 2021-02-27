open Boost

type 'a observer = {
  read: 'a -> unit;
  window: Time.window;
  loc: Unique.t;
  mutable isActive: bool;
}

type 'a t = 'a observer

let unsub obs = obs.isActive <- false

let defaultGen = Unique.makeWithLabel ~label:"obs"

let inspect v ~depth ~options =
  if depth < 0 then options##stylize "[reader]" `special else
    match v with
      | { loc; isActive = false } ->
          let s = Format.sprintf
            "{ Observer %s [inactive] }"
            (Unique.toString loc)
          in options##stylize s `undefined
      | { window = (t1, t2); loc } ->
          let t1 = Inspect.time t1 ~depth ~options in
          let t2 = Inspect.time t2 ~depth ~options in
          Format.sprintf "{ Observer %s from %s to %s }"
            (options##stylize (Unique.toString loc) `string)
            t1
            t2

let make ?label read window =
  let gen = match label with
    | Some (label) -> Unique.makeWithLabel ~label
    | None -> defaultGen
  in
  let loc = Unique.value gen in
  let obs = { read; window; loc; isActive = true } in
  if Flags.pretty_output then Inspect.setInspector obs (inspect obs) else obs
            