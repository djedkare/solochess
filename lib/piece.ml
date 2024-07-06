open Format

type t =
  | K
  | Q
  | R
  | B
  | N
  | P

let pp fmt p =
  fprintf
    fmt
    (match p with
     | K -> "K"
     | Q -> "Q"
     | R -> "R"
     | B -> "B"
     | N -> "N"
     | P -> "P")
;;

let pieces = [ K; Q; R; B; N; P ]
