(* Inc.ml Copyright (c) 2021 Diao Zheng *)

(** Yalib-inc is adapted from Self-Adjusting Computation by Umut Acar *)

include Combinators
include Meta_

module Var = Var
module Emitter = Emitter
module Box = Box

module Observer = Observer
module Promise = Promise

include Map

let (>>>) = Var.(>>>)
