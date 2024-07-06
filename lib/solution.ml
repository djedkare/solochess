module PositionPuzzle = struct
  type t =
    { position : Position.t
    ; history : Move.t list
    }

  let is_solved p = Position.is_solved p.position

  let next_move { position = p; history = h } =
    let f m = { position = Position.apply_move p m; history = m :: h } in
    Seq.map f (Position.moves p)
  ;;
end

let make p = PositionPuzzle.{ position = p; history = [] }
let get_history x = PositionPuzzle.(x.history) |> List.rev

module ChessBacktracking = Backtracking.Make (PositionPuzzle)

let solve p = p |> make |> ChessBacktracking.solve |> Seq.map get_history |> List.of_seq

let solve_first p =
  p
  |> make
  |> ChessBacktracking.solve
  |> Seq.uncons
  |> Option.map (fun (p2, _) -> get_history p2)
;;
