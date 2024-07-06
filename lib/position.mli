module BoardMap : Map.S with type key = int * int

type t = (Piece.t * int) BoardMap.t

exception SquareEmpty of (int * int)
exception WrongPiece of (int * int)
exception PieceExhausted of (int * int)
exception KingCaptured of (int * int)

val pp_position : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
val make : (Piece.t * (int * int)) list -> t
val make_ : (int * Piece.t * (int * int)) list -> t
val equal : t -> t -> bool
val equivalent : t -> t -> bool
val same_pieces : t -> t -> bool
val is_solved : t -> bool
val apply_move : t -> Move.t -> t
val reachable_squares_king : t -> int * int -> (int * int) Seq.t
val moves_from_square : t -> int * int -> Move.t Seq.t
val moves : t -> Move.t Seq.t
