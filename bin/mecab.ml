open! Batteries
open! Unix
module UC = BatUChar
module Utf8 = BatUTF8

type ty =
  | Doushi
  | Joudoushi
  | Other
[@@deriving show]

type seg =
  { readings : (string * string option) list
  ; ty : ty
  } [@@deriving show]

(* TODO: this is very janky and expensive, fix this properly with FFI in the future *)
let parse s =
  let (output , input) = open_process "mecab" in
  IO.write_line input s;
  IO.flush input;
  let rec loop results =
    match String.trim (IO.read_line output) with
    | "EOS" -> results
    | s ->
       let (word , result_s) = String.rsplit s ~by:"\t" in
       let result = String.split_on_char ',' result_s in
       let result =
         match result with
         | hinshi :: hinshibunrui1 :: _hinshibunrui2 :: _hinshibunrui3
           :: _katsuyougata :: _katsuyoukei :: genkei :: yomi :: hatsuon :: _
           -> ( hinshi , hinshibunrui1 , genkei , yomi , hatsuon )
         | _ -> failwith (Printf.sprintf "Invalid mecab output format: %s" result_s)
       in
       loop ((word , result) :: results)
  in
  let results = List.rev (loop []) in
  let hiraganalize (c : UC.t) =
    let code = UC.code c in
    let open Int in
    let code =
      (* Katakana range: U+30A1 - U+30F6 *)
      if code >= 12449 && code <= 12534
      then code - 96 (* Hiragana range: U+3041 - U+3096 *)
      else code
    in UC.chr code
  in
  let results =
    List.map
      (fun (word , ( hinshi , _ , _ , yomi , _ )) -> 
        let readings =
          match yomi with
          (* no reading *)
          | "*" 
            (* exactly equal, probably for katakana *)
            | _ when String.equal yomi word -> [ ( word , None ) ]
          | _ ->
             let rec rev_match word_index yomi_index =
               match word_index , yomi_index with
               | 0 , _ | _ , 0 -> 
                  (* in which case entire reading should just be directly trimmed *)
                  [ ( word , None ) ]
               | _ ->
                  let wordc = Utf8.get word (word_index - 1) |> hiraganalize in
                  let yomic = Utf8.get yomi (yomi_index - 1) |> hiraganalize in
                  if UC.eq wordc yomic
                  then
                    rev_match (word_index - 1) (yomi_index - 1)
                  else
                    let word_index = Utf8.nth word word_index
                    and yomi_index = Utf8.nth yomi yomi_index
                    in
                    let preword = String.slice ~last:word_index word
                    and preyomi = String.slice ~last:yomi_index yomi
                    and postword = String.slice ~first:word_index word in
                    [ ( preword , Some (Utf8.map hiraganalize preyomi) ) ;
                      ( postword , None )
                    ]
             in
             rev_match (Utf8.length word) (Utf8.length yomi)
        in
        let ty =
          match hinshi with
          | "動詞" -> Doushi
          | "助動詞" -> Joudoushi
          | _ -> Other
        in { readings ; ty }
      )
      results
  in
  IO.close_in output;
  IO.close_out input;
  results
