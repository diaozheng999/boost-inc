type index = int
type 'a box = { label: int; value: 'a }

type 'a t = 'a box

let next = ref 0

let init () = next := -1

let create v =
  next := (!next) + 1;
  { label = !next; value = v }

let fromInt i = { label = i; value = i }

let fromOption ob =
  match ob with
    | None -> { label = -1; value = ob }
    | Some( { label }) -> { label; value = ob }

let eq {label = ka} {label = kb} = ka = kb

let valueOf { value } = value

let indexOf { label } = label
