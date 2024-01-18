open! Batteries
open! Unix
module UC = BatUChar

type ty =
  | Doushi
  | Joudoushi
  | Other
[@@deriving show]

type unicode_string =
  string [@printer Format.pp_print_string]
    [@@deriving show]

type seg =
  { readings : (unicode_string * unicode_string option) list
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
           :: _katsuyougata :: _katsuyoukei :: genkei :: rest
           ->
            let yomi , hatsuon =
              match rest with
              | yomi :: hatsuon :: _ -> (Some yomi , Some hatsuon)
              | _ -> (None , None)
            in
            ( hinshi , hinshibunrui1 , genkei , yomi , hatsuon )
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
          | None | Some "*" -> [ ( word , None ) ]
          (* exactly equal, probably for katakana *)
          | Some yomi when String.equal yomi word -> [ ( word , None ) ]
          | Some yomi ->
             let rec rev_match rword ryomi leftover =
               match rword, ryomi with
               | [] , _ | _ , [] ->
                  (* in which case entire reading should just be directly trimmed *)
                  [ ( word , None ) ]
               | ((a :: rword) as allrword) , ((b :: ryomi) as allryomi) ->
                  if UC.eq (hiraganalize a) b
                  then
                    rev_match rword ryomi (a :: leftover)
                  else
                    [ ( Utf8.implode (List.rev allrword) ,
                        Some (Utf8.implode (List.rev allryomi)) ) ;
                      ( Utf8.implode leftover , None )
                    ]
             in
             rev_match
               (List.rev (Utf8.explode word))
               (List.rev (List.map hiraganalize (Utf8.explode yomi)))
               []
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
