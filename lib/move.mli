type t =
  { piece : Piece.t
  ; origin : int * int
  ; destination : int * int
  }

val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
val pp_variation : Format.formatter -> t list -> unit [@@ocaml.toplevel_printer]
