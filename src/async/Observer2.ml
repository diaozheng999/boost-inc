open Boost

type 'a observer = {
  id: Unique.t;
  next: ('a -> unit Js.Promise.t [@bs]);
  window: Time.window;
}

type 'a static_observer = {
  id: Unique.t;
  on_change: ('a -> unit [@bs]);
  once: bool;
  mutable is_active: bool;
}

let default_gen = Unique.make_with_label ~label:"obs2"

let static_uncurried ?label ?(once=false) on_change =
  let gen = match label with
    | Some label -> Unique.make_with_label ~label
    | None -> default_gen
  in
  let id = Unique.value gen in
  { id; once; on_change; is_active = true }

let inc_uncurried ?label ~window next =
  let gen = match label with
    | Some label -> Unique.make_with_label ~label
    | None -> default_gen
  in
  let id = Unique.value gen in
  { id; next; window }

let static ?label ?once on_change =
  static_uncurried ?label ?once (Bs_interop.arity1 on_change)

let inc ?label ~window next =
  inc_uncurried ?label ~window (Bs_interop.arity1 next)
