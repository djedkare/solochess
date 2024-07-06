open Format

module Coordinate = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
  ;;

  let on_board (x, y) = (x >= 0 && x <= 7) && y >= 0 && y <= 7
  let all_squares = Seq.init 64 (fun n -> n / 8, n mod 8)
end

module BoardMap = Map.Make (Coordinate)

type t = (Piece.t * int) BoardMap.t

exception SquareEmpty of (int * int)
exception PieceExhausted of (int * int)
exception WrongPiece of (int * int)
exception KingCaptured of (int * int)

let string_of_map_entry entry =
  match entry with
  | None -> "."
  | Some (Piece.K, 0) -> "#"
  | Some (_, 0) -> "0"
  | Some (K, 1) -> "k"
  | Some (Q, 1) -> "q"
  | Some (R, 1) -> "r"
  | Some (B, 1) -> "b"
  | Some (N, 1) -> "n"
  | Some (P, 1) -> "p"
  | Some (K, 2) -> "K"
  | Some (Q, 2) -> "Q"
  | Some (R, 2) -> "R"
  | Some (B, 2) -> "B"
  | Some (N, 2) -> "N"
  | Some (P, 2) -> "P"
  | Some _ -> "X"
;;

let pp_position fmt pos =
  fprintf fmt "\n";
  for y = 7 downto 0 do
    for x = 0 to 7 do
      BoardMap.find_opt (x, y) pos |> string_of_map_entry |> fprintf fmt "%s"
    done;
    fprintf fmt "\n"
  done
;;

let is_solved pos = BoardMap.cardinal pos = 1

let make lst =
  List.fold_left (fun bm (p, (x, y)) -> BoardMap.add (x, y) (p, 2) bm) BoardMap.empty lst
;;

let make_ lst =
  List.fold_left
    (fun bm (n, p, (x, y)) -> BoardMap.add (x, y) (p, n) bm)
    BoardMap.empty
    lst
;;

let equal pos0 pos1 = BoardMap.equal ( = ) pos0 pos1

(* don't expect all won positions to be equivalent! *)
(* this uses sq as a name for what's on a square, not the square coords! *)
let equivalent pos0 pos1 =
  let eqv sq0 sq1 =
    match sq0, sq1 with
    | (p0, 0), (p1, 0) -> Bool.equal (p0 = Piece.K) (p1 = Piece.K)
    | _ -> sq0 = sq1
  in
  BoardMap.equal eqv pos0 pos1
;;

let same_pieces pos0 pos1 =
  let same_piece (p0, _) (p1, _) = p0 = p1 in
  BoardMap.equal same_piece pos0 pos1
;;

(** does not notice if the piece on [origin] can't actually go to [destination] *)
let apply_move pos Move.{ origin = o; destination = d; piece = p } =
  match BoardMap.find_opt o pos with
  | None -> raise (SquareEmpty o)
  | Some (p_, n) ->
    if n <= 0
    then raise (PieceExhausted o)
    else if p <> p_
    then raise (WrongPiece o)
    else (
      match BoardMap.find_opt d pos with
      | None -> raise (SquareEmpty d)
      | Some (Piece.K, _) -> raise (KingCaptured d)
      | Some _ -> BoardMap.remove o (BoardMap.add d (p, n - 1) pos))
;;

let is_valid_target pos sq =
  match BoardMap.find_opt sq pos with
  | None -> false
  | Some (Piece.K, _) -> false
  | Some _ -> true
;;

let king_seq = Utils.seq_of_list [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]

let knight_seq =
  Utils.seq_of_list [ -2, -1; -2, 1; -1, -2; -1, 2; 1, -2; 1, 2; 2, -1; 2, 1 ]
;;

let pawn_seq = Utils.seq_of_list [ 1, 1; -1, 1 ]

let rec target_in_direction pos sq direction =
  let sq = Utils.add_pair sq direction in
  if is_valid_target pos sq
  then Seq.(cons sq empty)
  else if not (Coordinate.on_board sq)
  then Seq.empty
  else target_in_direction pos sq direction
;;

let bishop_direction_seq = Utils.seq_of_list [ -1, -1; -1, 1; 1, -1; 1, 1 ]
let rook_direction_seq = Utils.seq_of_list [ -1, 0; 0, -1; 0, 1; 1, 1 ]
let queen_direction_seq = Seq.append bishop_direction_seq rook_direction_seq

let reachable_squares_king pos sq =
  king_seq |> Seq.map (Utils.add_pair sq) |> Seq.filter (is_valid_target pos)
;;

let reachable_squares_knight pos sq =
  knight_seq |> Seq.map (Utils.add_pair sq) |> Seq.filter (is_valid_target pos)
;;

let reachable_squares_pawn pos sq =
  pawn_seq |> Seq.map (Utils.add_pair sq) |> Seq.filter (is_valid_target pos)
;;

let reachable_squares_queen pos sq =
  queen_direction_seq
  |> Seq.flat_map (fun direction -> target_in_direction pos sq direction)
;;

let reachable_squares_rook pos sq =
  rook_direction_seq
  |> Seq.flat_map (fun direction -> target_in_direction pos sq direction)
;;

let reachable_squares_bishop pos sq =
  bishop_direction_seq
  |> Seq.flat_map (fun direction -> target_in_direction pos sq direction)
;;

let moves_from_square pos sq =
  match BoardMap.find_opt sq pos with
  | None -> Seq.empty
  | Some (p, n) ->
    if n <= 0
    then Seq.empty
    else
      Seq.map
        (fun target_sq -> Move.{ piece = p; origin = sq; destination = target_sq })
        ((match p with
          | Piece.K -> reachable_squares_king
          | Piece.N -> reachable_squares_knight
          | Piece.P -> reachable_squares_pawn
          | Piece.Q -> reachable_squares_queen
          | Piece.R -> reachable_squares_rook
          | Piece.B -> reachable_squares_bishop)
           pos
           sq)
;;

let moves pos =
  Coordinate.all_squares |> Seq.flat_map (fun sq -> moves_from_square pos sq)
;;
