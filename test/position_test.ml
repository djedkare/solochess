open Solochess
open Position
open Utils
open OUnit2

let sq = square

let assert_same_elements ?printer l1 l2 =
  assert_equal ?printer (List.sort compare l1) (List.sort compare l2)
;;

let assert_equal_position ?msg pos0 pos1 =
  assert_equal ~printer:(fun pos -> Utils.str_fn_of_pp pp_position pos) ?msg pos0 pos1
;;

let assert_equal_current_position ?msg pos0 pos1 =
  assert_equal ~printer:(fun pos -> Utils.str_fn_of_pp pp_position pos) ?msg pos0 pos1
;;

(*  *)
let is_solved_cases =
  List.map
    (fun p ->
      "is_solved correctly identifies a solve with remaining piece "
      ^ Format.asprintf "%a" Piece.pp p
      >:: fun _ ->
      assert_bool
        ("is_solved does not correctly identify a solve with remaining piece "
         ^ Format.asprintf "%a" Piece.pp p)
        (make [ p, sq "d6" ] |> is_solved))
    Piece.pieces
  @ [ ("is_solved correctly identifies an unsolved position"
       >:: fun _ ->
       assert_bool
         "is_solved does not correctly identify an unsolved position"
         (make [ Piece.Q, sq "c8"; Piece.N, sq "h7" ] |> is_solved |> not))
    ]
;;

let pos0 = make [ Piece.Q, (2, 2); Piece.N, (5, 5); Piece.K, (5, 6) ]
let apply_move_cases = []

let equivalent_cases =
  [ ("" >:: fun _ -> assert_bool "" (equivalent (make_ []) (make_ [])))
  ; (""
     >:: fun _ ->
     assert_bool "" (not (equivalent (make_ [ 0, Piece.N, (2, 2) ]) (make_ []))))
  ; (""
     >:: fun _ ->
     assert_bool
       ""
       (not (equivalent (make_ [ 0, Piece.N, (2, 2) ]) (make_ [ 0, Piece.Q, (3, 2) ]))))
  ; (""
     >:: fun _ ->
     assert_bool
       ""
       (equivalent (make_ [ 0, Piece.N, (2, 2) ]) (make_ [ 0, Piece.Q, (2, 2) ])))
  ; (""
     >:: fun _ ->
     assert_bool
       ""
       (not (equivalent (make_ [ 0, Piece.N, (2, 2) ]) (make_ [ 0, Piece.K, (2, 2) ]))))
  ]
;;

let king_reachable_squares_cases =
  [ (""
     >:: fun _ ->
     assert_same_elements
       (reachable_squares_king (make [ K, sq "c3" ]) (sq "c3") |> List.of_seq)
       [])
  ; (""
     >:: fun _ ->
     assert_same_elements
       (reachable_squares_king
          (make [ K, sq "c3"; Q, sq "d4"; N, sq "c4"; N, sq "c5" ])
          (sq "c3")
        |> List.of_seq)
       [ sq "d4"; sq "c4" ])
  ]
;;

let moves_from_square_cases =
  [ (""
     >:: fun _ ->
     assert_same_elements
       (moves_from_square (make [ K, sq "c3" ]) (sq "c3") |> List.of_seq)
       [])
  ; (""
     >:: fun _ ->
     assert_same_elements
       (moves_from_square
          (make [ K, sq "c3"; Q, sq "d4"; N, sq "c4"; N, sq "c5" ])
          (sq "c3")
        |> List.of_seq)
       [ { piece = Piece.K; origin = sq "c3"; destination = sq "d4" }
       ; { piece = Piece.K; origin = sq "c3"; destination = sq "c4" }
       ])
  ; (""
     >:: fun _ ->
     assert_same_elements
       (moves_from_square
          (make [ K, sq "c3"; Q, sq "d4"; N, sq "c4"; N, sq "c5" ])
          (sq "c5")
        |> List.of_seq)
       [])
  ; (""
     >:: fun _ ->
     assert_same_elements
       (moves_from_square
          (make [ K, sq "c3"; Q, sq "d4"; N, sq "c4"; N, sq "c5" ])
          (sq "d4")
        |> List.of_seq)
       [ { piece = Piece.Q; origin = sq "d4"; destination = sq "c5" }
       ; { piece = Piece.Q; origin = sq "d4"; destination = sq "c4" }
       ])
  ; (""
     >:: fun _ ->
     assert_same_elements
       (moves_from_square
          (make [ R, sq "c3"; Q, sq "d4"; N, sq "c4"; N, sq "c5" ])
          (sq "d4")
        |> List.of_seq)
       [ { piece = Piece.Q; origin = sq "d4"; destination = sq "c5" }
       ; { piece = Piece.Q; origin = sq "d4"; destination = sq "c4" }
       ; { piece = Piece.Q; origin = sq "d4"; destination = sq "c3" }
       ])
  ; (""
     >:: fun _ ->
     assert_same_elements
       (moves_from_square
          (make [ R, sq "c3"; B, sq "d4"; N, sq "c4"; N, sq "c5" ])
          (sq "d4")
        |> List.of_seq)
       [ { piece = Piece.B; origin = sq "d4"; destination = sq "c5" }
       ; { piece = Piece.B; origin = sq "d4"; destination = sq "c4" }
       ; { piece = Piece.B; origin = sq "d4"; destination = sq "c3" }
       ])
  ]
;;

let cases =
  is_solved_cases @ apply_move_cases @ equivalent_cases @ king_reachable_squares_cases
;;
