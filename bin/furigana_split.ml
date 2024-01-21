open! Batteries
module UC = BatUChar

(* Katakana range: U+30A1 - U+30F6 *)
let katakana_range code = code >= 12449 && code <= 12534
(* Hiragana range: U+3041 - U+3096 *)
let hiragana_range code = code >= 12353 && code <= 12438

let hiraganalize (c : UC.t) =
  let code = UC.code c in
  let open Int in
  let code =
    if katakana_range code
    then code - 96
    else code
  in UC.chr code

module type SPLIT = sig
  type t

  val init : unit -> t
  val split : t -> jishokei:string -> base:string -> reading:string -> Annot.word
end

module RefuseBasicAnnotation (S : SPLIT) : SPLIT = struct
  include S

  let split t ~jishokei ~base ~reading =
    if List.for_all
         (fun c ->
           let code = UC.code c in
           let open Int in
           List.exists (fun f -> f code)
             [ katakana_range
             ; hiragana_range
             ]
         )
         (Utf8.explode base)
    then Annot.of_segs [ (base , None) ]
    else split t ~jishokei ~base ~reading
end

module NaiveSplit : SPLIT = struct
  type t = unit

  let init () = ()
  
  let split () ~jishokei:_ ~base ~reading =
    let rec rev_match rword ryomi leftover =
      match rword, ryomi with
      | [] , _ | _ , [] ->
         (* in which case entire reading should just be directly trimmed *)
         [ ( base , None ) ]
      | ((a :: rword) as allrword) , ((b :: ryomi) as allryomi) ->
         if UC.eq (hiraganalize a) b
         then
           rev_match rword ryomi (a :: leftover)
         else
           let head = 
             ( Utf8.implode (List.rev allrword) ,
               Some (Utf8.implode (List.rev allryomi)) )
           in
           match leftover with
           | [] -> [ head ]
           | _ -> 
              [ head ;
                ( Utf8.implode leftover , None )
              ]
    in
    rev_match
      (List.rev (Utf8.explode base))
      (List.rev (List.map hiraganalize (Utf8.explode reading)))
      []
    |> Annot.of_segs
end

module JmdictFuriganaSplit_(Input : sig val filename : string end) = struct
  open Input
  module H = Hashtbl.Make(String)

  type entry =
    { segs : (string * string option) list;
    }

  type t =
    { dictionary : entry H.t ;
      (* the typing is bizzare because this actually works like an association list *)
    }

  type json_seg =
    { base : string [@key "ruby"] ;
      reading : string option [@key "rt"][@default None] ;
    } [@@deriving of_yojson { exn = true }]

  type json_entry =
    { text : string ;
      reading : string ;
      furigana : json_seg list;
    } [@@deriving of_yojson { exn = true }]

  type json_file =
    json_entry list
      [@@deriving of_yojson { exn = true }]

  let init () =
    let entries =
      File.with_file_in filename
      (fun input ->
        let s = IO.read_all input in
        (* for reasons due to lord knows why, the default file has these bizarre
           preceeding characters that we need to trim *)
        let i = String.index s '[' in
        let s = String.slice ~first:i s in
        Yojson.Safe.from_string s
        |> json_file_of_yojson_exn
      )
    in
    let dictionary = H.create (List.length entries) in
    let () =
      List.iter
        (fun { text ; reading = _ ; furigana } ->
          let segs = List.map (fun { base ; reading } -> (base , reading)) furigana in
          let entry = { segs } in
          H.add dictionary text entry
        )
        entries
    in { dictionary }
    
  let split { dictionary } ~jishokei ~base ~reading =
    let results = H.find_all dictionary jishokei in
    let candidates =
      List.filter_map
        (fun { segs } ->
          let kanji_root segs =
            let rec h segs =
              match segs with
              | [] -> []
              | (_ , None) :: rest ->
                 h rest
              | (_ , Some _) :: _ ->
                 List.rev segs
            in
            h (List.rev segs)
          in
          let segs = kanji_root segs in
          let r =
            List.map
              (fun (base , reading) ->
                match reading with
                | None -> base
                | Some s -> s)
              segs |> String.concat ""
          in
          if String.starts_with_stdlib ~prefix:r reading
          then
            Some
              begin
                (* match and trim rest *)
                let rest = String.slice ~first:(String.length r) reading in
                match String.length rest with
                | 0 -> segs
                | _ -> segs @ [ (rest , None) ]
              end
          else None
        )
        results
    in
    begin
    match List.rev candidates with
    | [] -> [ ( base , Some reading ) ] (* we fail and don't spilt *)
    | v :: _ -> v
    end
    |> Annot.of_segs
end

module JmdictFuriganaSplit(Input : sig val filename : string end) : SPLIT =
  RefuseBasicAnnotation(JmdictFuriganaSplit_(Input))

module PickBest (A : SPLIT) (B : SPLIT) : SPLIT = struct
  type t = A.t * B.t

  let init () = (A.init () , B.init ())

  let split (at , bt) ~jishokei ~base ~reading =
    let a = A.split at ~jishokei ~base ~reading
    and b = B.split bt ~jishokei ~base ~reading in
    if List.length (Annot.segs a) < List.length (Annot.segs b)
    then b else a
end