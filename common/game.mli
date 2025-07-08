open! Core

type t = {
  game_kind : Game_kind.t;
  board : Piece.t Position.Map.t;
  last_move : (Piece.t * Position.t) option;
  prev_state : t option;
}
[@@deriving sexp_of, bin_io]

val empty : Game_kind.t -> t
val set_piece : t -> Position.t -> Piece.t -> t
val get_board : t -> Piece.t Position.Map.t
val get_game_kind : t -> Game_kind.t
val get_last_move : t -> (Piece.t * Position.t) option
val get_prev_state : t -> t option
