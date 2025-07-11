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
      | true, _, _ -> true
      | _, false, _ -> false
      | _, _, None -> false
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

(*(* Exercise 6 *)
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
      | _ -> List.hd_exn all_pos)*)

(* Exercise 7 *)

let available_adjacents ~node =
  available_moves node
  |> List.filter ~f:(fun pos ->
         List.exists Position.all_offsets ~f:(fun dir ->
             match Map.find (Game.get_board node) (dir pos) with
             | Some _ -> true
             | None -> false))

let uncapped_triples node =
  let board = Game.get_board node in
  let all_pos = all_positions node in

  let check_triple pos dir1 dir2 =
    match
      (Map.find board pos, Map.find board (dir1 pos), Map.find board (dir2 pos))
    with
    | Some piece0, Some piece1, Some piece2 ->
        Piece.equal piece0 piece1 && Piece.equal piece0 piece2
    | _ -> false
  in

  let check_all pos =
    if check_triple pos Position.down Position.up then
      (true, Position.down, Position.up)
    else if check_triple pos Position.right Position.left then
      (true, Position.right, Position.left)
    else if
      check_triple pos
        (Fn.compose Position.up Position.right)
        (Fn.compose Position.down Position.left)
    then
      ( true,
        Fn.compose Position.up Position.right,
        Fn.compose Position.down Position.left )
    else if
      check_triple pos
        (Fn.compose Position.up Position.left)
        (Fn.compose Position.down Position.right)
    then
      ( true,
        Fn.compose Position.up Position.left,
        Fn.compose Position.down Position.right )
    else (false, Position.up, Position.up)
  in

  let capped pos dir piece =
    match Map.find board ((Fn.compose dir dir) pos) with
    | None ->
        not
          (Position.in_bounds
             ((Fn.compose dir dir) pos)
             ~game_kind:Game_kind.Omok)
    | Some piece2 -> not (Piece.equal piece piece2)
  in

  let triples =
    List.fold all_pos ~init:[] ~f:(fun total pos ->
        let has_triple, dir1, dir2 = check_all pos in
        if Map.mem board pos && has_triple then
          let piece = Option.value_exn (Map.find board pos) in
          if (not (capped pos dir1 piece)) && not (capped pos dir2 piece) then
            (pos, true, dir1, dir2) :: total
          else if not (capped pos dir1 piece) then
            (pos, false, dir1, dir1) :: total
          else if not (capped pos dir2 piece) then
            (pos, false, dir2, dir2) :: total
          else total
        else total)
  in
  triples

let score ~(node : Game.t) =
  if Game_kind.equal Game_kind.Tic_tac_toe (Game.get_game_kind node) then 0
  else
    let board = Game.get_board node in
    let all_pos = all_positions node in
    let centering =
      List.fold all_pos ~init:0 ~f:(fun total pos ->
          let closeness = 98 - Position.from_center pos in
          match Map.find board pos with
          | Some piece ->
              if Piece.equal Piece.X piece then total + closeness
              else total - closeness
          | None -> total)
    in
    let triple_score =
      List.fold (uncapped_triples node) ~init:0
        ~f:(fun total (pos, double, _, _) ->
          match (double, Option.value_exn (Map.find board pos)) with
          | true, Piece.X -> total + 3
          | true, Piece.O -> total - 3
          | false, Piece.X -> total + 1
          | false, Piece.O -> total - 1)
    in
    let final = centering + (triple_score * 1000) in
    (*print_endline (Int.to_string final);*)
    final

let rec minimax ~(node : Game.t) ~(depth : int) ~(player : Piece.t) : int =
  if depth = 0 then score ~node
  else
    match evaluate node with
    | Illegal_move -> failwith "unexpected illegal move"
    | Game_over { winner = Some piece } ->
        if Piece.equal Piece.X piece then Int.max_value else Int.min_value
    | Game_over { winner = None } -> 0
    | Game_continues ->
        let center = Position.{ row = 7; column = 7 } in
        let search_next =
          match Map.find (Game.get_board node) center with
          | None -> center :: available_adjacents ~node
          | _ -> available_adjacents ~node
        in
        let opposing_triples =
          List.filter (uncapped_triples node) ~f:(fun (pos, double, _, _) ->
              double
              && Piece.equal (Piece.flip player)
                   (Option.value_exn (Map.find (Game.get_board node) pos)))
        in
        let search_next =
          match opposing_triples with
          | [] -> search_next
          | triples ->
            List.fold triples ~init:[]
                ~f:(fun total (pos, double, dir1, dir2) ->
                  match double with
                  | true ->
                      (Fn.compose dir1 dir1) pos
                      :: (Fn.compose dir2 dir2) pos
                      :: total
                  | false -> total)
        in
        List.fold
          (List.map search_next ~f:(fun pos -> Game.set_piece node pos player))
          ~init:
            (match player with Piece.X -> Int.min_value | _ -> Int.max_value)
          ~f:(fun acc child ->
            (match player with Piece.X -> Int.max | _ -> Int.min)
              acc
              (minimax ~node:child ~depth:(depth - 1)
                 ~player:(Piece.flip player)))

let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let all_pos = available_adjacents ~node:game in
  let all_pos =
    match all_pos with
    | [] -> [ Position.{ row = 7; column = 7 } ]
    | _ -> all_pos
  in
  let move, _ =
    match you_play with
    | Piece.X ->
        List.fold all_pos
          ~init:(List.hd_exn all_pos, Int.min_value)
          ~f:(fun (hipos, hiscore) pos ->
            let score =
              minimax
                ~node:(Game.set_piece game pos you_play)
                ~depth:2 ~player:(Piece.flip you_play)
            in
            if score > hiscore then (pos, score) else (hipos, hiscore))
    | Piece.O ->
        List.fold all_pos
          ~init:(List.hd_exn all_pos, Int.max_value)
          ~f:(fun (hipos, hiscore) pos ->
            let score =
              minimax
                ~node:(Game.set_piece game pos you_play)
                ~depth:2 ~player:(Piece.flip you_play)
            in
            if score < hiscore then (pos, score) else (hipos, hiscore))
  in
  move
