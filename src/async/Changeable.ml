open Boost

type 'a read_spec

type 'a action =
  | Read of 'a read_spec
  | Write of 'a
  | Act of string * (('a Variable.t -> unit Js.Promise.t)[@bs])

type 'a t = {
  name : Unique.t;
  action : 'a action;
  stack : Unique.t array option;
}

external pack_read_spec :
  variable:'a Variable.t -> next:('a -> 'b t) -> 'b read_spec = ""
  [@@bs.obj]


external get_variable : 'a read_spec -> 'b Variable.t = "variable" [@@bs.get]

external get_next : 'a read_spec -> ('b -> 'a t[@bs]) = "next" [@@bs.get]

type abs_t

external __abs : 'a t -> abs_t = "%identity"

external __real : abs_t -> 'a t = "%identity"

let keep_last_n_frames = 20

let frame_count = ref 0

let execution_context : abs_t Linked_list.t = Linked_list.make ()

module Stack = struct

  let push stack value =
    match stack with
      | Some stack ->
        let _ : int = Js.Array2.push stack value in
        stack
      | None -> [| value |]

  let get stack =
    match stack with
      | Some stack -> Some (Belt.Array.copy stack)
      | None -> None

end

let gen = Unique.make_with_label ~label:"action"

let build_stack changeable =
  match changeable.stack with
  | Some arr when Js.Array2.length arr > 0 ->
      Js.Array2.joinWith arr "/" ^ "/" ^ Unique.to_str changeable.name
  | _ -> Unique.to_str changeable.name


let update_execution_context changeable =
  let _ = Linked_list.add_to_front execution_context (__abs changeable) in
  if !frame_count >= keep_last_n_frames then
    let _ = Linked_list.remove_from_end execution_context in ()
  else
    frame_count := !frame_count + 1

let rec act changeable dest =
  update_execution_context changeable;
  match changeable.action with
  | Read reader ->
      let variable = get_variable reader in
      let next = get_next reader in
      Variable.read ~label:(build_stack changeable) variable (fun x ->
          act (next x [@bs]) dest)
  | Write value -> Variable.write dest value
  | Act (_, f) -> f dest [@bs]

let get_name label =
  match label with Some label -> label | None -> Unique.value gen

let get_stack = Stack.get

let read ?label ?stack variable next =
  {
    name = get_name label;
    stack = get_stack stack;
    action = Read (pack_read_spec ~variable ~next);
  }

let write ?label ?stack value =
  { name = get_name label; stack = get_stack stack; action = Write value }

let make ?label ?stack ?kind action =
  let name = get_name label in
  {
    name;
    stack = get_stack stack;
    action = Act (Belt.Option.getWithDefault kind (Unique.to_str name), action);
  }

let variable changeable =
  let label = build_stack changeable in
  let label = Unique.make_with_label ~label in
  let variable = Variable.make_intermediate ~label () in
  act changeable variable
  |> Bs_interop.unstable_promise_then_unit_exec (fun () -> variable)


let to_string changeable = 
  let name = build_stack changeable in
  let action =
    match changeable.action with
      | Read spec ->
        let v = get_variable spec in
        let name = Unique.to_str v.Variable.name in
        {j|Read $(name)|j}
      | Write _ -> "Write"
      | Act (label, _) -> {j|Act $(label)|j}
  in
  {j|$(action) ($(name))|j}

let rec print_exec n node =
  let open Linked_list in
  match n, node with
    | _, None -> ()
    | 0, Some _ -> Js.log "..."
    | n, Some { value; next; _ } ->
      Js.log2 n (to_string (__real value));
      print_exec (n - 1) next

let log_execution_context () =
  Js.log "Execution Context";
  print_exec (!frame_count) (Linked_list.head execution_context)
