open! Batteries
module E = Epub
module M = Mecab
module A = Annot
module FS = Furigana_split

let prompt_line s =
  let open BatIO in
  List.iter (write_line stdout) s;
  flush stdout;
  let s = read_line stdin in
  String.trim s

type prompt_response =
  | Keep of bool
  | Mecab of bool
  | Custom of bool * A.word

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
                      A.to_string annots ;
                      "? " ;
                      "Mecab Reference: ";
                      A.to_string mecab ;
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
    | s ->
       let word = A.of_string s in
       let rec promptdict () =
         match
           prompt_line
             [ "Propagate to dictionary?" ;
               "[ (y)es / (n)o / (b)ackout to retype / default to yes ]" ;
             ]
         with
         | "y" | "" -> Custom (true , word)
         | "n" -> Custom (false , word)
         | "b" -> loop ()
         | _ ->
            BatIO.write_line stdout "Unknown option";
            BatIO.flush stdout;
            promptdict ()
       in promptdict ()
  in
  loop ()

type state =
  { last_line : E.content list option
  (* TODO : think about the right data structure here, probably trie *)
  ; dictionary : A.word list
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
  let dicopt =
    Opt.value_option
      "DICTIONARY_PATH"
      (Some None)
      (fun s ->
        File.with_file_in s
          ~mode:File.[ `create ]
          ~perm:(File.unix_perm 0o644)
          (fun input -> 
           let data =
             IO.read_all input
             |> String.split_on_char '\n'
             |> List.filter_map
                  (fun s ->
                    let s = String.trim s in
                    let open Int in
                    if String.length s == 0
                    then None
                    else Some s)
             |> List.map A.of_string
           in Some (s , data)
      ))
      (fun exn v ->
        Printf.sprintf
          "Cannot open file '%s': %s"
          v (Printexc.to_string exn)
      )
  in
  let outopt =
    StdOpt.str_option ~metavar:"OUTPUT" ()
  in
  let () =
    P.add parser ~help:"Path to epub file"
      ~short_names:['i']
      ~long_names:["path" ; "input" ; "in"]
      pathopt;
    P.add parser ~help:"Path to local dictionary file"
      ~short_names:['d']
      ~long_name:"local"
      dicopt;
    P.add parser ~help:"Path to output file"
      ~short_names:['o']
      ~long_names:["output" ; "out"]
      outopt;
    ()
  in
  let _ = P.parse_argv parser in
  let file = Opt.get pathopt in
  let dictionary , push_write_entry =
    match Opt.get dicopt with
    | None -> [] , fun _ -> ()
    | Some (dicfile , dic) ->
       dic ,
       fun entry ->
       File.with_file_out
         ~mode:File.[ `append ]
         dicfile
         (fun out -> IO.write_line out (A.to_string entry))
  in
  let module S = FS.JmdictFuriganaSplit(struct let filename = "test.json" end) in
  let t = S.init () in
  let result = S.split t ~jishokei:"基本" ~base:"基本" ~reading:"きほん" in
  let () = print_endline (A.show_word result) in
  let state = { last_line = None ; dictionary } in
  let () =
    E.fold_map_content
      (Opt.get outopt)
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
        let push_entry entry =
          dictionary := entry :: !dictionary;
          push_write_entry entry
        in

        (* rewriting the stored term into the tree *)
        let rewrite_annotate word (content : E.content list) =
          let annot_base = A.base word in
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
                            ) (A.segs word)) @
                       (split after term rest)
               in
               let result = split s 0 positions in
               result
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
               | [] | (Text _) :: _ | (Tag _) :: _ ->
                  ( annots
                    |> List.rev
                    |> List.map (fun (a , b) -> (a , Some b))
                    |> A.of_segs
                  , c)
               | (Annotate (a , b)) :: rest -> collect_annot rest ((a , b) :: annots) 
             in
             let annot , rest = collect_annot rest [(a , b)] in
             (* There are a whole bunch of crazy cases that can theoretically happen and this depends on how accurate mecab is *)
             (* Overall strategy: we consider the book as the source of truth and that to be a single word.
                There are bizarre cases where it is not a single word and then its much more complicated. *)
             let annot_base = A.base annot
             and annot_reading = A.annot annot in
             (* print_endline ([%show: M.seg list] parsing); *)
             (* print_endline (A.show_word annot); *)
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
             let mecab_reading = A.of_segs mecab_reading in
             let map_annot (a , b) =
               match b with
               | None -> E.Text a
               | Some b -> Annotate (a , b)
             in
             let use_annotate annots =
               (List.map map_annot (List.rev (A.segs annots))) @ result
             in
             if String.starts_with_stdlib ~prefix:annot_reading mecab_reading_s
             then (* we're actually good here, just use annotations as is *)
               inject rest parsing (use_annotate annot)
             else
               (* if its already in the dictionary, we just skip, even if the annotation is different *)
               if List.exists (fun word -> String.equal annot_base (A.base word)) !dictionary
               then
                 inject rest parsing (use_annotate annot)
               else
               (* otherwise we prompt for reading conflict *)
               let response = prompt annot mecab_reading in
               let write_dict_and_parse dic annot =
                  let rest =
                    if dic
                    then
                      begin
                        push_entry annot;
                        let rest = rewrite_annotate annot rest in
                        rest
                      end
                    else rest
                  in
                  inject rest parsing (use_annotate annot)
               in
               match response with
               | Keep dic ->
                  write_dict_and_parse dic annot
               | Mecab dic ->
                  write_dict_and_parse dic mecab_reading
               | Custom (dic , annot) ->
                  write_dict_and_parse dic annot
        in
        let (result , _) = inject content parsed [] in
        (* let () = print_endline ([%show: E.content list] result) in *)
        let state =
          { state with
            last_line = Some content ;
            dictionary = !dictionary ;
          }
        in result , state) state
      file
  in
  ()
