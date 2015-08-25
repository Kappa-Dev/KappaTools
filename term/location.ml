type 'a annot = 'a * (Lexing.position * Lexing.position)
type 'a maybe = ?pos:(Lexing.position * Lexing.position) -> 'a

let dummy_annot x = (x, (Lexing.dummy_pos, Lexing.dummy_pos))
let has_dummy_annot (_,(b_pos,e_pos)) =
  b_pos = Lexing.dummy_pos &&
    (e_pos = Lexing.dummy_pos || failwith "half dummy_pos")
