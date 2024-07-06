open OUnit2

let suite = "suite" >::: Position_test.cases (* Piece_test.cases *)
let () = run_test_tt_main suite
