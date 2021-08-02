open Boost

external finally : ('a -> unit) -> 'a Promise.t = "finally"

  [@@bs.send.pipe: 'a Promise.t]


type 'a t = {
  mutex : Mutex.t;
  mutable value : int;
  mutable locked_by : 'a option;
}

let make ~mutex = { mutex; value = 0; locked_by = None }


let try_release mut =
  mut.value <- mut.value - 1;
  if mut.value == 0 then
    let () = mut.locked_by <- None in
    Mutex.release mut.mutex

let after_acquire ~mutex ~owner fn =
  mutex.value <- mutex.value + 1;
  mutex.locked_by <- Some owner;
  (fn () [@bs]) |> finally (fun _ -> try_release mutex)

let acquire ~mutex ~owner fn =
  match (mutex.value, mutex.locked_by) with
  | curr_value, Some current_owner when current_owner == owner && curr_value > 0
    ->
      after_acquire ~mutex ~owner fn
  | _ ->
      Mutex.acquire mutex.mutex
      |> Js.Promise.then_ (fun () -> after_acquire ~mutex ~owner fn)
