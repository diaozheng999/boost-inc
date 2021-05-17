(* Flag.ml Copyright (c) 2021 Diao Zheng *)

(** Flags.ml - place to hold debug flags statically. *)

(** Set to `true` to enable debug logging of Combinators *)
let debug_combinators = false

(** Set to `true` to enable debug logging of the `propagate` function *)
let debug_propagate = false

(** Set to `true` to enable debug logging of `Memo_table` lookups *)
let debug_memotable = false

(** Set to `true` to enable condensed printing for command line using the
    `Inspect` utility module *)
let pretty_output = false

(** Set to `true` to allow actual epoch timestamps to be used to generate
    timestamps. Uses float values otherwise. *)
let real_time = true

(** Set to `true` to enable debug logging of `Redux` driver *)
let debug_redux = false

(** Set this to `true` if logging in React Native (or other environments) where
    `inspect.custom` symbol is not present. *)
let inspect_with_polyfill = true
