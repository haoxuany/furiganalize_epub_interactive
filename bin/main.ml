open! Batteries
module E = Epub
module M = Mecab

let prompt_line s =
  let open BatIO in
  List.iter (write_line stdout) s;
  flush stdout;
  let s = read_line stdin in
  String.trim s

type prompt_response =
  | Keep of bool
  | Mecab of bool

let prompt_annotation lines annots mecab =
  let rec loop () =
    match
      prompt_line
        begin
          "-----------" ::
            "Context:" ::
              (lines @
                 [
                   "" ;
                   String.concat ""
                     ["What do you do with " ;
                      String.concat ""
                        (List.map (fun (base, annot) ->
                             String.concat "" ["[" ; base ; "]" ; "(" ; annot ; ")"])
                           annots) ;
                      "? " ;
                      "Mecab Reference: ";
                      String.concat ""
                        (List.map
                           (fun (base, annot) ->
                             match annot with
                             | None -> base
                             | Some s ->
                                String.concat ""
                                  ["[" ; base ; "]" ; "(" ; s ; ")"])
                           mecab) ;
                     ] ;
                   "[ (k)eep orignal / (kd) keep in dictionary and propagate / defaults to keep ]" ;
                   "[ (m) use mecab / (md) use mecab in dictionary / provide your own correction otherwise ]" ;
                 ]
              )
        end
    with
    | "k" | "" -> Keep false
    | "kd" -> Keep true
    | "m" -> Mecab false
    | "md" -> Mecab true
    | _ -> BatIO.write_line stdout "Unknown option"; loop ()
  in
  loop ()

type word = (string * string option) list
type state =
  { last_line : E.content list option
  (* TODO : think about the right data structure here, probably trie *)
  ; dictionary : (string * word) list
  }

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
  let state = { last_line = None ; dictionary = [] } in
  let () =
    E.fold_map_content
      "output.epub"
      (fun state content ->
        let context =
          begin
          match state.last_line with
          | None -> [ content ]
          | Some l -> [ l ; content ]
          end
          |> List.map E.show_plain_content
        in
        let prompt = prompt_annotation context in

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

        let dictionary = ref state.dictionary in

        (* rewriting the stored term into the tree *)
        let rewrite_annotate (annot_base , annot) (content : E.content list) =
          let annot_len = String.length annot_base in
          let rec rewrite (content : E.content) : E.content list =
            match content with
            | Text s ->
               let positions = String.find_all s annot_base |> List.of_enum in
               let rec split s i positions =
                 match positions with
                 | [] -> [ E.Text s ]
                 | pos :: rest ->
                    if pos < i then
                      split s i rest
                    else
                      let before = String.slice ~last:(pos - i) s  in
                      let term = (pos - i) + annot_len in
                      let after = String.slice ~first:term s in
                      (E.Text before)
                      :: (List.map
                            (fun (a , b) ->
                              match b with
                              | None -> E.Text a
                              | Some b -> Annotate (a , b)
                            ) annot) @
                       (split after term rest)
               in
               split s 0 positions
            | Annotate _ -> [ content ]
            | Tag (a , b , children) ->
               [ Tag (a , b , List.concat_map rewrite children) ]
          in List.concat_map rewrite content
        in

        let content =
          List.fold_left
            (fun content annot -> rewrite_annotate annot content)
            content (!dictionary)
        in
        (* let () = print_endline ([%show: E.content list] content) in *)
        (* inject mecab *)
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
          | (Annotate (a, b)) :: rest , parsing ->
             let rec collect_annot (c : E.content list) annots =
               match c with
               | [] | (Text _) :: _ | (Tag _) :: _ -> (List.rev annots , c)
               | (Annotate (a , b)) :: rest -> collect_annot rest ((a , b) :: annots) 
             in
             let annots , rest = collect_annot rest [(a , b)] in
             (* There are a whole bunch of crazy cases that can theoretically happen and this depends on how accurate mecab is *)
             (* Overall strategy: we consider the book as the source of truth and that to be a single word.
                There are bizarre cases where it is not a single word and then its much more complicated. *)
             let annot_base, annot_reading = List.split annots in
             let annot_base = String.concat "" annot_base
             and annot_reading = String.concat "" annot_reading in
             (* print_endline ([%show: M.seg list] parsing); *)
             (* TODO: optimize this with substrings *)
             let rec collect_mecab_reading (parsing : M.seg list) base
                     : (string * string option) list * M.seg list =
               match String.length base with
               | 0 -> ([] , parsing)
               | _ ->
                  begin
                    match parsing with
                    | ({ readings = [] ; _ } :: parsing) ->
                       collect_mecab_reading parsing base
                    | ({ readings = (word , reading) :: readingrest ; ty } :: parsing) ->
                       if String.starts_with_stdlib ~prefix:word base (* partial matches *)
                       then
                         let (matches , parsing) =
                           collect_mecab_reading ({ readings = readingrest ; ty } :: parsing)
                             (String.slice ~first:(String.length word) base)
                         in
                         ((word , reading) :: matches , parsing)
                       else
                         if String.starts_with_stdlib ~prefix:base word (* match overflows *)
                         then
                           ([(word , reading)] , { readings = readingrest ; ty } :: parsing )
                         else
                           (* mismatch otherwise, this is just really bad and likely a bug within mecab itself *)
                           (* try and junk and then pray *)
                           ([(word , reading)] , { readings = readingrest ; ty } :: parsing )
                    | [] -> ([] , [])
                  end
             in
             let mecab_reading , parsing = collect_mecab_reading parsing annot_base in
             (* we do a extremely stupid match here, if it doesn't work, we say screw it and then prompt *)
             let mecab_reading_s =
               mecab_reading
               |> List.map (fun (_ , reading) -> match reading with None -> "?" | Some s -> s)
               |> String.concat ""
             in
             let use_annotate () =
               (List.map (fun (a, b) -> E.Annotate (a, b)) annots) @ result
             in
             let use_mecab () =
               (List.map
                  (fun (a , b) ->
                    match b with
                    | None -> E.Text a
                    | Some b -> Annotate (a , b))
                  mecab_reading) @ result
             in
             if String.starts_with_stdlib ~prefix:annot_reading mecab_reading_s
             then (* we're actually good here, just use annotations as is *)
               inject rest parsing (use_annotate ())
             else
               (* if its already in the dictionary, we just skip, even if the annotation is different *)
               if List.exists (fun (s , _) -> String.equal annot_base s) !dictionary
               then
                 inject rest parsing (use_annotate ())
               else
               (* otherwise we prompt for reading conflict *)
               let response = prompt annots mecab_reading in
               match response with
               | Keep dic ->
                  let rest =
                    if dic
                    then
                      let annots = List.map (fun (a , b) -> (a , Some b)) annots in
                      let d = (annot_base , annots) in
                      dictionary := d :: !dictionary;
                      let rest = rewrite_annotate d rest in
                      rest
                    else rest
                  in
                  inject rest parsing (use_annotate ())
               | Mecab dic ->
                  let rest =
                    if dic
                    then
                      let d = (mecab_reading_s , mecab_reading) in
                      dictionary := d :: !dictionary;
                      let rest = rewrite_annotate d rest in
                      rest
                    else rest
                  in
                  inject rest parsing (use_mecab ())
        in
        let (result , _) = inject content parsed [] in
        let state =
          { state with
            last_line = Some content ;
            dictionary = !dictionary ;
          }
        in result , state) state
      file
  in
  ()
