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
  { word : unicode_string
  ; reading : unicode_string option
  ; jishokei : unicode_string
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
  let results =
    List.map
      (fun (word , ( hinshi , _ , genkei , yomi , _ )) -> 
        let reading =
          match yomi with
          | None | Some "*" -> None
          | Some s ->
             s
             |> Utf8.explode
             |> List.map Kana.hiraganalize
             |> Utf8.implode
             |> fun s -> Some s
        in
        let jishokei = genkei in
        let ty =
          match hinshi with
          | "動詞" -> Doushi
          | "助動詞" -> Joudoushi
          | _ -> Other
        in { word ; reading ; jishokei ; ty }
      )
      results
  in
  IO.close_in output;
  IO.close_out input;
  results
