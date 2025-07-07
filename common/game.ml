open! Core
open! Async

type t = { game_kind : Game_kind.t; board : Piece.t Position.Map.t }
[@@deriving sexp_of, bin_io]

let empty game_kind = { game_kind; board = Position.Map.empty }

let set_piece t position piece =
  { t with board = Map.set t.board ~key:position ~data:piece }

let get_board t = t.board
let get_game_kind t = t.game_kind
