open Format

type t =
  { piece : Piece.t
  ; origin : int * int
  ; destination : int * int
  }

let pp fmt mv =
  Piece.pp fmt mv.piece;
  Utils.pp_square fmt mv.origin;
  fprintf fmt "x";
  Utils.pp_square fmt mv.destination
;;

let pp_variation fmt hist =
  let pp_n n =
    if n > 0 then fprintf fmt " ";
    fprintf fmt "%d. " (n + 1)
  in
  List.iteri
    (fun n move ->
      pp_n n;
      pp fmt move)
    hist
;;
