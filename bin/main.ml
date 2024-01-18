open! Batteries
module E = Epub
module M = Mecab

let () =
  let open BatOptParse in
  let module P = OptParser in
  let parser =
    P.make
      ~prog:"fei"
      ~description:"Annotate EPUB files with furigana"
      ()
  in
  let pathopt =
    Opt.value_option
      "PATH"
      None
      E.from_file
      (fun exn v ->
        Printf.sprintf
          "Cannot open file '%s': %s"
          v (Printexc.to_string exn)
      )
  in
  let () =
    P.add parser ~help:"Path to epub file" ~long_name:"path" pathopt;
    ()
  in
  let _ = P.parse_argv parser in
  let file = Opt.get pathopt in
  let () =
    E.fold_map_content
      "output.epub"
      (fun state content ->
        let rec strip (c : E.content) =
          match c with
          | Text s -> s
          | Annotate (s , _) -> s
          | Tag (_ , _ , children) ->
             List.map strip children
             |> String.concat ""
        in
        let text =
          List.map strip content
          |> String.concat ""
        in
        let parsed = M.parse text in
        let rec inject content (parsing : M.seg list) result =
          match content , parsing with
          | [] , parsing -> (List.rev result , parsing)
          | content , [] ->
          (* Not sure why mecab didn't pick it up, hard dump everything else *)
             (List.concat [ List.rev result ; content ] , [])
          | (E.Tag (name , attrs , content)) :: rest , parsing ->
             let (children , parsing) = inject content parsing [] in
             inject rest parsing (E.Tag (name , attrs , children) :: result)
          | content , { readings = [] ; _ } :: parsing ->
             inject content parsing result
          | (Text s) :: rest , (({ readings = (word , reading) :: readingrest ; ty } :: parsing) as allparsing) ->
             begin
               match Utf8.explode s with
               (* Case 1: too late to match, look at next token *)
               | [] -> inject rest allparsing result
               (* Case 2: segment matches *)
               | _ when String.starts_with_stdlib ~prefix:word s ->
                  let srest = String.slice ~first:(String.length word) s in
                  inject ((Text srest) :: rest) ({ readings = readingrest ; ty } :: parsing)
                    begin
                      (match reading with
                       | None -> Text word
                       | Some reading -> Annotate (word , reading))
                      :: result
                    end
               (* Case 3: segment half matches [very tricky], this is likely a parsing error of mecab *)
               (* or a way too creative author *)
               | _ when String.starts_with_stdlib ~prefix:s word ->
                  (* TODO: we junk both for time being, perhaps think of a better way of handling this or
                     figure out scenarios when this happens *)
                  inject rest ({ readings = readingrest ; ty } :: parsing) ((Text s) :: result)
               (* Case 4: segment doesn't match, in which case we dump a character and look further ahead *)
               | c :: srest ->
                  inject ((Text (Utf8.implode srest)) :: rest) allparsing ((Text (Utf8.of_char c)) :: result)
             end
          | (Annotate (a, b)) :: rest , (({ readings = (word , reading) :: readingrest ; ty } :: parsing) as allparsing) ->
             begin
               match Utf8.explode a with
               (* Case 1: too late to match, look at next token *)
               | [] -> inject rest allparsing result
               (* Case 2: segment matches *)
               | _ when String.starts_with_stdlib ~prefix:word a ->
                  (* TODO: we junk both for time being, perhaps think of a better way of handling this or
                     figure out scenarios when this happens *)
                  inject rest ({ readings = readingrest ; ty } :: parsing) ((Annotate (a, b)) :: result)
               (* Case 3: segment half matches [very tricky], this is likely a parsing error of mecab *)
               (* or a way too creative author *)
               | _ when String.starts_with_stdlib ~prefix:a word ->
                  (* TODO: we junk both for time being, perhaps think of a better way of handling this or
                     figure out scenarios when this happens *)
                  inject rest ({ readings = readingrest ; ty } :: parsing) ((Annotate (a, b)) :: result)
               (* Case 4: segment doesn't match, in which case we dump a character and look further ahead *)
               | c :: srest ->
                  (* TODO: we junk both for time being, perhaps think of a better way of handling this or
                     figure out scenarios when this happens *)
                  inject rest ({ readings = readingrest ; ty } :: parsing) ((Annotate (a, b)) :: result)
             end
        in
        let (result , _) = inject content parsed [] in
        result , state) ()
      file
  in
  ()
