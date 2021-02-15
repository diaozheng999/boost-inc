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

let time = (t: time) => (~depth as _, ~options) => {
  let major = withOptions(t.value.at, ~options)
  let minor = options["stylize"](.`${Js.Float.toString(t.value.sub)}`, #undefined)
  `${major}|${minor}`
}
