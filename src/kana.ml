open! BatUChar

let in_range (a , b) i = i >= a && i <= b
(* Referencing: https://stackoverflow.com/questions/19899554/unicode-range-for-japanese *)
(* Katakana range: U+30A1 - U+30FF *)
let code_katakana_range = in_range (0x30A1 , 0x30FF)
(* Hiragana range: U+3041 - U+3096 *)
let code_hiragana_range = in_range (0x3041 , 0x3096)
(* Half Width Kana, Punctuation: U+FF5F - U+FF9F *)
let code_halfwidth_punctuation_range = in_range (0xFF5F , 0xFF9F)
(* Symbols and Punctuation: U+3000 - U+303F *)
let code_punctuation_range = in_range (0x3000 , 0x303F)
(* Fullwidth range: U+FF01 - U+FF5E *)
let code_fullwidth_range = in_range (0xFF01 , 0xFF5E)
(* Miscellaneous Symbols and Characters: U+31F0 - U+31FF U+3220 - U+3243 U+3280 - U+337F] *)
let code_misc_range code =
  List.exists
    (fun r -> in_range r code)
    [ (0x31F0 , 0x31FF)
    ; (0x3220 , 0x3243)
    ; (0x3280 , 0x337F)
    ]

let hiraganalize (c : t) =
  let code = code c in
  let open Int in
  let code =
    if code_katakana_range code
    then code - 0x60
    else code
  in chr code

