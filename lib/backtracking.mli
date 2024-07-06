module type Puzzle = sig
  type t

  val is_solved : t -> bool
  val next_move : t -> t Seq.t
end

module type S = sig
  type puzzle

  val size_of_tree : puzzle -> int
  val size_up_to : int -> puzzle -> int list
  val solve : puzzle -> puzzle Seq.t
end

module Make (P : Puzzle) : S with type puzzle = P.t
