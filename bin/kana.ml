open! BatUChar

(* Katakana range: U+30A1 - U+30F6 *)
let code_katakana_range code = code >= 12449 && code <= 12534
(* Hiragana range: U+3041 - U+3096 *)
let code_hiragana_range code = code >= 12353 && code <= 12438

let hiraganalize (c : t) =
  let code = code c in
  let open Int in
  let code =
    if code_katakana_range code
    then code - 96
    else code
  in chr code

