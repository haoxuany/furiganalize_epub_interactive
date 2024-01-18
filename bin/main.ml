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
        content , state) ()
      file
  in
  ()
