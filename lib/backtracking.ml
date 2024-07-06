(* alternative: *)
(* type 'a result = Win | Loss | Branch of 'a Seq.t *)
(* module type Puzzle = sig type t; val check : t -> t result end *)
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

(* module Make (P : Puzzle) : S *)
(* doesn't match the interface *)
module Make (P : Puzzle) = struct
  type puzzle = P.t

  let rec solve p =
    if P.is_solved p then Seq.(cons p empty) else Seq.flat_map solve (P.next_move p)
  ;;

  let rec size_of_tree p =
    1
    +
    if P.is_solved p
    then 0
    else P.next_move p |> Seq.map size_of_tree |> Seq.fold_left ( + ) 0
  ;;

  let rec list_add l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | l1, [] -> l1
    | [], l2 -> l2
    | h1 :: t1, h2 :: t2 -> (h1 + h2) :: list_add t1 t2
  ;;

  let rec size_up_to n p =
    if n <= 0
    then []
    else 1 :: (P.next_move p |> Seq.map (size_up_to (n - 1)) |> Seq.fold_left list_add [])
  ;;
end
