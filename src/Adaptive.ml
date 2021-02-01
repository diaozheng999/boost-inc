type changeable = unit
exception Unset_mod

type edge = { reader: unit -> unit; timeInterval: (Time.t * Time.t) }

type 'a node = 
  { mutable value: unit -> 'a
  ; mutable wrt: 'a -> unit
  ; mutable out: edge list 
  }

type 'a modifiable = 'a node
type 'a dest = 'a node

let PQ = 

let modifiable cmp f =
  let m = {
    value=(fun () -> raise Unset_mod);
    wrt=(fun _ -> raise Unset_mod);
    out=[]
  } in
  let change t v =
    if cmp v (m.value()) then ()
    else (
      m.value <- fun () -> v;
      m.out <- [];
    ) in
    
  let write v =
    m.value <- fun () -> v in
  
  f m;
  m

let p: int node = {
  value=(fun () -> 5);
  wrt=(fun (_) -> ());
  out=[]
}

let _ = Js.Console.log p