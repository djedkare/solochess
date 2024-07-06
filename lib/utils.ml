let seq_of_list lst =
  let f lst =
    match lst with
    | [] -> None
    | h :: t -> Some (h, t)
  in
  Seq.unfold f lst
;;

let add_pair (x0, y0) (x1, y1) = x0 + x1, y0 + y1
let ( ++ ) a b = add_pair a b

let pp_square fmt (x, y) =
  Format.fprintf fmt "%c" (Char.chr (x + 97));
  Format.fprintf fmt "%d" (y + 1)
;;

let str_fn_of_pp pp = Format.asprintf "%a" pp

let square str =
  if String.length str > 2
  then raise (Failure "too long")
  else (
    let x = int_of_char (String.get str 0) - int_of_char 'a' in
    let y = int_of_char (String.get str 1) - int_of_char '1' in
    x, y)
;;
