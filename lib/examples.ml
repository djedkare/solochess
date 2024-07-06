open Piece

let sq = Utils.square
let ex3_0 = Position.make [ P, (3, 3); B, (2, 4); R, (4, 4) ]

let ex6_0 =
  (* has a king *)
  Position.make [ P, sq "a1"; K, sq "a3"; R, sq "a4"; Q, sq "b3"; N, sq "c2"; R, sq "g3" ]
;;

let ex6_1 =
  (* has no king *)
  Position.make [ Q, sq "b3"; Q, sq "b7"; Q, sq "c2"; B, sq "c4"; B, sq "d3"; Q, sq "g7" ]
;;

let ex11_0 =
  Position.make
    [ N, sq "a6"
    ; K, sq "a7"
    ; R, sq "a8"
    ; Q, sq "b2"
    ; Q, sq "b3"
    ; R, sq "b4"
    ; B, sq "b8"
    ; N, sq "c5"
    ; P, sq "d7"
    ; Q, sq "e5"
    ; R, sq "f8"
    ]
;;
