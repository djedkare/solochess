module PositionPuzzle : sig
  type t

  val is_solved : t -> bool
  val next_move : t -> t Seq.t
end

val make : Position.t -> PositionPuzzle.t
val get_history : PositionPuzzle.t -> Move.t list

module ChessBacktracking : Backtracking.S with type puzzle = PositionPuzzle.t

val solve : Position.t -> Move.t list list
val solve_first : Position.t -> Move.t list option
