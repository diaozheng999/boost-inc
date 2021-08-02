open Boost

val make2 :
  ?name:Unique.unique_gen ->
  'a Variable.t ->
  'b Variable.t ->
  ('a * 'b) Variable.t

val make3 :
  ?name:Unique.unique_gen ->
  'a Variable.t ->
  'b Variable.t ->
  'c Variable.t ->
  ('a * 'b * 'c) Variable.t
