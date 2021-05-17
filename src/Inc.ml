(* Inc.ml Copyright (c) 2021 Diao Zheng *)

(** Boost-inc is adapted from Self-Adjusting Computation by Umut Acar *)

include Combinators
include Meta_

module Var = Var
module Emitter = Emitter
module Box = Box

module Observer = Observer
module Promise = Promise

module Let_syntax = Let_syntax

include Map

let (>>>) = Var.(>>>)
