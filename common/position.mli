open! Core

(* Top-left is [{row = 0; column = 0}].
     row indexes increment downwards.
     column indexes increment rightwards. *)
type t = { row : int; column : int }
[@@deriving sexp_of, equal, bin_io, compare]

include Comparable.S_binable with type t := t

val to_string : t -> string
val in_bounds : t -> game_kind:Game_kind.t -> bool

val down : t -> t
(** [down t] is [t]'s downwards neighbor. *)

val right : t -> t
(** [right t] is [t]'s rightwards neighbor. *)

val up : t -> t
(** [up t] is [t]'s upwards neighbor. *)

val left : t -> t
(** [left t] is [t]'s leftwards neighbor. *)

val all_offsets : (t -> t) list
(** [all_offsets] is a list of functions to compute all 8 neighbors of a cell
    (i.e. left, up-left, up, up-right, right, right-down, down, down-left). *)

val of_tuple : int * int -> t
