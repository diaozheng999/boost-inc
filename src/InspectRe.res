open Inspect_internal
open Types_internal

let memotableEntry = e => (~depth, ~options) => {
  if depth < 0 {
    stylize(options, "[reader]", #special)
  } else {
    switch e.contents {
      | Some(_, None) => `reader at ${stylize(options, "undefined", #undefined)} time`
      | Some(_, Some(f, t)) =>
        `reader from ${withOptions(f, ~options)} to ${withOptions(t, ~options)}`
      | _ => stylize(options, "spliced out", #undefined)
    }
  }
}

let option = (inspectElement, opt) => (~depth, ~options) => {
  if depth < 0 {
    stylize(options, "[Option]", #special)
  } else {
    switch opt {
      | None => stylize(options, "None", #undefined)
      | Some(v) => `Some(${inspectElement(v, ~depth, ~options)})`
    }
  }
}

let ref = (inspectElement, ref) => (~depth, ~options) => {
  `${stylize(options, "#ref", #undefined)} ${inspectElement(ref.contents, ~depth, ~options)}`
}

let s = Js.Float.toString

let time = (t: time) => (~depth as _, ~options) => {
  if t.value.isSplicedOut {
    stylize(options, `t|${s(t.value.at)}|${s(t.value.sub)} (spliced out)`, #undefined)
  } else if Flags.real_time {
    let major = withOptions(t.value.at, ~options)
    let minor = stylize(options, `${s(t.value.sub)}`, #undefined)
    `t|${major}|${minor}`
  } else {
    `t|${withOptions(t.value.sub, ~options)}`
  }
}

let linkedList = (list) => (~depth, ~options) => {
  if depth < 0 {
    stylize(options, "LinkedList", #special)
  } else {
    let p = Boost.Linked_list.as_array_like(list) -> Js.Array.from
    withOptions(p, ~options)
  }
}
