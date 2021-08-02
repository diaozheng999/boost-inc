open Basis
open Boost

type 'a t = {
  name : Unique.t;
  mutable last_value : 'a option;
  mutable last_address : Address.t option;
  read : (unit -> ('a * Address.t) Js.Promise.t[@bs]);
  compute_address : ('a -> Address.t[@bs]);
  write : (Address.t -> 'a -> unit Js.Promise.t[@bs]);
  observers : 'a Observer2.observer Linked_list.t;
  stable_observers : 'a Observer2.static_observer Linked_list.t;
}

let intermediate_gen = Unique.make_with_label ~label:"intermediate"

let read_and_update_value value =
  match value.last_value with
  | Some v -> Js.Promise.resolve v
  | None ->
      (value.read () [@bs])
      |> Js.Promise.then_ (fun (resolved_value, addr) ->
             value.last_value <- Some resolved_value;
             value.last_address <- Some addr;
             Js.Promise.resolve resolved_value)

let fire_static_observers ~variable ~value =
  Linked_list.for_each_node variable.stable_observers ~f:(fun node ->
      node.value.on_change value [@bs];
      if node.value.once || not node.value.is_active then
        Linked_list.remove_node variable.stable_observers node)
  |> Js.Promise.resolve

let execute_current_observers ~variable ~value =
  Propagate.Task_queue.insert (fun [@bs] () ->
      fire_static_observers ~variable ~value);
  Linked_list.for_each_node variable.observers ~f:(fun node ->
      match node.value.window with
      | t1, _ when Time.isSplicedOut t1 ->
          Linked_list.remove_node variable.observers node
      | window ->
          Propagate.Task_queue.insert ~window
            (Bs_interop.bind node.value.next value))

let write variable value =
  let insert =
   fun [@bs] () ->
    let address = (variable.compute_address value [@bs]) in
    match variable.last_address with
    | None -> Js.Promise.resolve ()
    | Some last_addr when Address.eq last_addr address -> Js.Promise.resolve ()
    | Some _ ->
        let read () =
          variable.last_value <- Some value;
          variable.last_address <- Some address;
          execute_current_observers ~variable ~value
        in
        (variable.write address value [@bs])
        |> Bs_interop.unstable_promise_then_unit_exec read
  in

  let exec = fun [@bs] () ->
    Propagate.Task_queue.insert insert;
    Js.Promise.resolve ()
  in

  Propagate.when_not_propagating exec

let make_intermediate () =
  let read =
   fun [@bs] () ->
    failwith "Intermediates do not have an explicit `read` function."
  in
  let compute_address = fun [@bs] v -> Boost.Hash.hash v |> Address.int in
  let write = fun [@bs] _ _ -> Promise.resolve () in
  {
    name = Unique.value intermediate_gen;
    last_value = None;
    last_address = None;
    read;
    write;
    compute_address;
    observers = Linked_list.make ();
    stable_observers = Linked_list.make ();
  }

let make ?read ?write ?compute_address ?last_value ~name =
  let read =
    Belt.Option.getWithDefault read (fun [@bs] () ->
        let n = Unique.to_str name in
        failwith {j|Variable $(n) did not implement a read function. |j})
  in

  let write =
    Belt.Option.getWithDefault write (fun [@bs] _ _ -> Promise.resolve ())
  in

  let compute_address =
    Belt.Option.getWithDefault compute_address (fun [@bs] v ->
        Boost.Hash.hash v |> Address.int)
  in

  let last_address = Belt.Option.mapU last_value compute_address in
  {
    name;
    last_value;
    last_address;
    read;
    write;
    compute_address;
    observers = Linked_list.make ();
    stable_observers = Linked_list.make ();
  }
