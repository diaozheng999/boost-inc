
let (>>=) a b = Js.Promise.then_ b a
let (>>|) a b = Js.Promise.then_ (fun () -> b () |> Js.Promise.resolve) a
