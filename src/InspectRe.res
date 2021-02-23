open Inspect_internal
open Types_internal

let memotableEntry = e => (~depth, ~options) => {
  if depth < 0 {
    options["stylize"](."[reader]", #special)
  } else {
    switch e.contents {
      | Some(_, None) => `reader at ${options["stylize"](."undefined", #undefined)} time`
      | Some(_, Some(f, t)) =>
        `reader from ${withOptions(f, ~options)} to ${withOptions(t, ~options)}`
      | _ => options["stylize"](."spliced out", #undefined)
    }
  }
}

let option = (inspectElement, opt) => (~depth, ~options) => {
  if depth < 0 {
    options["stylize"](."[Option]", #special)
  } else {
    switch opt {
      | None => options["stylize"]("None", #undefined)
      | Some(v) => `Some(${inspectElement(v, ~depth, ~options)})`
    }
  }
}

let ref = (inspectElement, ref) => (~depth, ~options) => {
  `${options["stylize"](."#ref", #undefined)} ${inspectElement(ref.contents, ~depth, ~options)}`
}

let s = Js.Float.toString

let time = (t: time) => (~depth as _, ~options) => {
  if t.value.isSplicedOut {
    options["stylize"](.`t|${s(t.value.at)}|${s(t.value.sub)} (spliced out)`, #undefined)
  } else if Flags.real_time {
    let major = withOptions(t.value.at, ~options)
    let minor = options["stylize"](.`${s(t.value.sub)}`, #undefined)
    `t|${major}|${minor}`
  } else {
    `t|${withOptions(t.value.sub, ~options)}`
  }
}

let linkedList = (list) => (~depth, ~options) => {
  if depth < 0 {
    options["stylize"](."LinkedList", #special)
  } else {
    let p = Boost.LinkedList.asArrayLike(list) -> Js.Array.from
    withOptions(p, ~options)
  }
}
