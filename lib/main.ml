open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let all_positions game =
  let length = Game_kind.board_length (Game.get_game_kind game) in
  List.map
    (List.cartesian_product
       (List.init length ~f:(fun x -> x))
       (List.init length ~f:(fun x -> x)))
    ~f:Position.of_tuple

let print_row row board =
  let pre_processed_string =
    List.fold row ~init:"" ~f:(fun final_string pos ->
        final_string
        ^
        match Map.find board (Position.of_tuple pos) with
        | Some piece -> Piece.to_string piece ^ " | "
        | None -> "  | ")
  in
  print_endline
    (String.sub pre_processed_string ~pos:0
       ~len:(String.length pre_processed_string - 3))

let print_rowline row length board =
  print_endline (String.make length '-');
  print_row row board

let print_game (game : Game.t) =
  let length = Game_kind.board_length (Game.get_game_kind game) in
  let num_dashes = (4 * length) - 3 in
  let board = Game.get_board game in
  let all_pos =
    List.cartesian_product
      (List.init length ~f:(fun x -> x))
      (List.init length ~f:(fun x -> x))
  in
  let rows =
    List.groupi all_pos ~break:(fun i _ _ ->
        match i % length with 0 -> true | _ -> false)
  in
  match rows with
  | hd :: tl ->
      print_row hd board;
      List.iter tl ~f:(fun row -> print_rowline row num_dashes board)
  | [] -> failwith "no rows"

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  let board = Game.get_board game in
  let all_pos = all_positions game in
  List.fold all_pos ~init:[] ~f:(fun acc pos ->
      match Map.find board pos with Some _ -> acc | None -> pos :: acc)

(* Exercise 2 *)

let evaluate (game : Game.t) : Evaluation.t =
  let kind = Game.get_game_kind game in
  let win_length = Game_kind.win_length kind in
  let board = Game.get_board game in
  let all_pos = all_positions game in
  let all_in_bounds =
    Map.is_empty
      (Map.filter_keys board ~f:(fun key ->
           not (Position.in_bounds key ~game_kind:kind)))
  in
  if not all_in_bounds then Evaluation.Illegal_move
  else
    let rec win_from_pos ~target pos depth direction =
      match
        ( depth >= win_length,
          Position.in_bounds pos ~game_kind:kind,
          Map.find board pos )
      with
      | _, false, _ -> false
      | _, _, None -> false
      | true, _, Some piece -> Piece.equal piece target
      | false, _, Some piece ->
          Piece.equal piece target
          && win_from_pos ~target (direction pos) (depth + 1) direction
    in

    let winning_position =
      List.find all_pos ~f:(fun pos ->
          match Map.find board pos with
          | None -> false
          | Some piece ->
              let halfdirections = Position.half_offsets in
              List.exists halfdirections ~f:(fun direction ->
                  win_from_pos ~target:piece pos 0 direction))
    in

    match winning_position with
    | Some pos -> Evaluation.Game_over { winner = Map.find board pos }
    | None ->
        if Map.length board = win_length * win_length then
          Evaluation.Game_over { winner = None }
        else Evaluation.Game_continues

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let all_pos = all_positions game in
  List.fold all_pos ~init:[] ~f:(fun acc pos ->
      match evaluate (Game.set_piece game pos me) with
      | Evaluation.Game_over { winner = Some piece } ->
          if Piece.equal piece me then pos :: acc else acc
      | _ -> acc)

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let all_pos = all_positions game in
  List.fold all_pos ~init:[] ~f:(fun acc pos ->
      match winning_moves ~me:(Piece.flip me) (Game.set_piece game pos me) with
      | _ :: _ -> pos :: acc
      | _ -> acc)

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let all_pos = available_moves game in
  let wins = winning_moves ~me:you_play game in
  match wins with hd :: _ -> hd | _ -> List.hd_exn all_pos

(* Exercise 6 *)
let make_move_one_ahead ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let all_pos = available_moves game in
  let wins = winning_moves ~me:you_play game in
  let losses = losing_moves ~me:you_play game in
  match wins with
  | hd :: _ -> hd
  | _ -> (
      match
        List.filter all_pos ~f:(fun pos ->
            not (List.exists losses ~f:(Position.equal pos)))
      with
      | hd :: _ -> hd
      | _ -> List.hd_exn all_pos)

(* Exercise 7 *)

let rec minimax ~(node : Game.t) ~(depth : int) ~(initial_player : Piece.t)
    ~(max : bool) ~(alpha : int) ~(beta : int) : int =
  if depth = 0 then 0
  else
    match evaluate node with
    | Illegal_move -> failwith "unexpected illegal move"
    | Game_over { winner = Some piece } ->
        if Piece.equal initial_player piece then Int.max_value
        else Int.min_value
    | Game_over { winner = None } -> 0
    | Game_continues -> (
        match max with
        | true ->
            List.fold
              (List.map (available_moves node) ~f:(fun pos ->
                   Game.set_piece node pos (Piece.flip initial_player)))
              ~init:Int.min_value
              ~f:(fun acc child ->
                Int.max acc
                  (minimax ~node:child ~depth:(depth - 1) ~initial_player
                     ~max:false))
        | false ->
            List.fold
              (List.map (available_moves node) ~f:(fun pos ->
                   Game.set_piece node pos initial_player))
              ~init:Int.max_value
              ~f:(fun acc child ->
                Int.min acc
                  (minimax ~node:child ~depth:(depth - 1) ~initial_player
                     ~max:true)))

let score ~(node : Game.t) ~(turn : Piece.t) =
  if Game_kind.equal Game_kind.Tic_tac_toe (Game.get_game_kind node) then 0
  else 0
