type t =
  | K
  | Q
  | R
  | B
  | N
  | P

val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
val pieces : t list
