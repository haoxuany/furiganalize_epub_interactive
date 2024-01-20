open! Batteries
module Z = Zip
module M = Markup

type infile =
  { filename : string
  ; file : Z.in_file
  ; spine : string list
  }

module OpfXhtmlMap = Map.Make(String)
type opf_data =
  { xhtml_indicies : string OpfXhtmlMap.t
  ; xhtml_order : string list
  }

let parse filename file =
  (* OPF >2.0.1 only *)
  (* We skip checking the mimetype file, since it's not too useful *)
  (* We look at contianer directly, this is a very small file so loading directly to memory *)
  let container = Z.find_entry file "META-INF/container.xml" in
  let container = Z.read_entry file container in
  let container = M.parse_xml (M.string container) in
  let fullpath =
    M.tree (M.signals container)
      ~element:
      begin
        fun name attrs children ->
        match name with
        | _ , "rootfile" ->
           List.find_map_opt 
             (fun ((_ , name) , value) ->
               match name with
               | "full-path" -> Some value
               | _ -> None)
             attrs
        | _ -> List.find_map_opt (fun i -> i) children
      end
  in
  let fullpath =
    match fullpath with
    | Some (Some p) -> p
    | _ -> failwith (Printf.sprintf "Cannot find full-path in META-INF/container.xml")
  in
  (* The OPF file is probably also very small, hence we load directly to memory *)
  let opf = Z.find_entry file fullpath in
  let opf = Z.read_entry file opf in
  let opf = M.parse_xml (M.string opf) in
  let spine =
    let empty =
      { xhtml_indicies = OpfXhtmlMap.empty
      ; xhtml_order = []
      }
    in
    (* due to List.foldr, order is reversed *)
    let merge b a =
      { xhtml_indicies = OpfXhtmlMap.union (fun _ a _ -> Some a) a.xhtml_indicies b.xhtml_indicies
      ; xhtml_order = List.append a.xhtml_order b.xhtml_order
      }
    in
    M.tree (M.content (M.signals opf))
      (* TODO: ideally this is handwritten (faster), since this is a very silly way of implementing this *)
      ~element:(
        fun (_ , name) attrs children : opf_data ->
        match name with
        | "package" | "metadata" | "meta" | "manifest" | "spine"
          -> List.fold_right merge children empty
        | "item" ->
           begin
             let result = 
             List.fold_left
               (fun (a , b , ok) ((_ , name) , value) ->
                 match name with
                 | "id" -> (Some value , b , ok)
                 | "href" -> (a , Some value , ok)
                 | "media-type" -> (a , b , String.equal value "application/xhtml+xml")
                 | _ -> (a , b , ok)
               )
               (None , None , false)
             attrs
             in
             { xhtml_indicies =
                 begin
                 match result with
                 | Some id , Some href , true -> OpfXhtmlMap.singleton id href
                 | _ -> OpfXhtmlMap.empty
                 end
             ; xhtml_order = []
             }
           end
        | "itemref" ->
           let idref = 
             List.find_map_opt
               (fun ((_ , name) , value) -> if String.equal name "idref" then Some value else None)
             attrs
           in
             { xhtml_indicies = OpfXhtmlMap.empty
             ; xhtml_order =
                 match idref with
                 | None -> []
                 | Some p -> [p]
             }
        | _ -> empty
      )
  in
  let { xhtml_indicies ; xhtml_order } =
    match spine with
    | None -> failwith (Printf.sprintf "Unable to find spine information in %s" fullpath)
    | Some s -> s
  in
  let spine =
    (* this is tricky, opf defines it here as the relative path to the opf file, so here
       we need to handle the path correctly *)
    let relpath =
      let open BatPathGen.OfString in
      let opfpath = of_string fullpath |> parent in
      fun s ->
      concat opfpath (of_string s) |> normalize |> to_string
    in
    List.map
      (fun s ->
        match OpfXhtmlMap.find_opt s xhtml_indicies with
        | None -> failwith (Printf.sprintf "Incorrectly defined spine: id %s not found" s)
        | Some s -> relpath s
      )
      xhtml_order
  in
  { filename ; file ; spine }

let from_file filename : infile =
  let file = Z.open_in filename in
  parse filename file

type content =
  | Text of string
  (* this is pretty restrictive since we assume that we only care about ruby tags. *)
  (* in practice this is very much true. *)
  | Annotate of string * string
  (* Any other tag should be preserved unless there's extremely good reason not to *)
  (* realistically probably some CSS class *)
  | Tag of M.name * (M.name * string) list * content list

let show_plain_content (c : content list) =
  let rec show c result =
    match c with
    | (Text s) :: rest -> show rest (s :: result)
    | (Annotate (a , b)) :: rest ->
       show rest ((String.concat "" [a ; "(" ; b ; ")"]) :: result)
    | (Tag (_ , _ , children)) :: rest ->
       show (children @ rest) result
    | [] -> String.concat "" (List.rev result)
  in show c []

let fold_map_content
      (output : string)
      (f : 'a -> content list -> content list * 'a)
      (initial : 'a)
      ({ filename ; file ; spine } : infile) : unit
  =
  let output = Z.open_out output in
  let entries = Z.entries file in
  let state = ref initial in

  let entries =
    List.map
      (fun (entry : Z.entry) ->
        (entry , 
         let open BatPathGen.OfString in
         entry.filename
         |> of_string
         |> normalize
         |> to_string
      ) )
      entries
  in
  let other_files , spine_files =
    let module SpineSet = Hashtbl.Make(String) in
    let spineset = SpineSet.of_list (List.map (fun s -> (s, ())) spine) in
    List.partition
      (fun (_ , name) -> 
        match SpineSet.find_option spineset name with
        | None -> true
        | Some () -> false)
      entries
  in
  (* first copy over other files *)
  let () =
    List.iter
      (fun (entry , _) ->
        (* do a direct copy *)
        (* TODO: this is jank, we're doing a full memory read here, optimize this *)
        Z.add_entry
          (Z.read_entry file entry)
          output entry.filename
      )
      other_files
  in
  (* Then handle spine files by spine order *)
  let spine_files =
    List.map
      (fun name ->
        let (file , _) =
          List.find (fun (_ , entry_name) -> String.equal name entry_name)
            spine_files
        in
        file
      )
      spine
  in
  let () =
    List.iter
      (fun (entry : Z.entry) ->
        (* do a fold write *)
        (* TODO: this is jank, we're doing a full memory read here, optimize this *)
        let file = Z.read_entry file entry in
        let content = M.parse_xml (M.string file) in
        let content = M.signals content in
        let queue : M.signal list ref = ref [] in
        let pop () =
          match !queue with
          | [] -> None
          | tok :: rest -> queue := rest; Some tok
        in
        let stream =
          M.stream
            (fun () ->
              match pop () with
              | Some s -> Some s
              | None -> 
                 let signal = M.next content in
                 match signal with
                 | None -> None
                 (* We only care about content within p tags in epubs *)
                 | Some (`Start_element ((_ , "p") , attrs)) ->
                    let rec parse_text text =
                      match M.next content with
                      | None -> text
                      | Some (`Text s) -> parse_text (Some (String.concat "" s))
                      | Some `End_element -> text
                      | Some _ ->
                         (* this will be crazy malformed, remove and pray *)
                         parse_text text
                    in
                    let rec parse_ruby result =
                      match M.next content with
                      | None -> result
                      | Some `End_element -> result
                      | Some `Text s -> parse_ruby (Text (String.concat "" s) :: result)
                      | Some (`Start_element ((_, "rb") , _)) ->
                         (* bizarrely some books use the rb tag for the regular bottom text (ex. Danganronpa Kirigiri 1) *)
                         let result = parse_ruby result in
                         parse_ruby result
                      | Some (`Start_element ((_, "rt") , _)) ->
                         begin
                           match parse_text None with
                           | None -> parse_ruby result
                           | Some text ->
                              (* Go back and find an annotation if it makes sense. If it doesn't then just delete *)
                              let result =
                                match result with
                                | (Text s) :: rest -> (Annotate (s , text)) :: rest
                                | _ -> result
                              in
                              parse_ruby result
                         end
                      | Some (`Start_element _)
(* this really isn't supposed to happen in html spec, we parse then delete *)
                        -> let result = parse_ruby result in
                           parse_ruby result
                      | Some (`Comment _) | Some (`Doctype _) | Some (`PI _) | Some (`Xml _)
                                                                                    (* bizarre, just delete *)
                        -> parse_ruby result
                    in
                    let rec parse_until result =
                      match M.next content with
                      | None -> result
                      | Some `End_element -> result
                      | Some (`Text s) -> parse_until (Text (String.concat "" s) :: result)
                      | Some (`Start_element ((_, "ruby") , _)) ->
                         let result = parse_ruby result in
                         parse_until result
                      | Some (`Start_element (name , attrs)) ->
                         let subtree = parse_until [] in
                         parse_until (Tag (name , attrs , subtree) :: result)
                      | Some (`Comment _) | Some (`Doctype _) | Some (`PI _) | Some (`Xml _)
                                                                                    (* bizarre, just delete *)
                        -> parse_until result
                    in
                    let parsed = parse_until [] |> List.rev in
                    let (parsed , s) = f (!state) parsed in
                    let () = state := s in
                    let rec transform content =
                      match content with
                      | Text s -> [`Text [s]]
                      | Annotate (a , b) ->
                         [`Start_element (("" , "ruby") , []) ;
                          `Text [a] ;
                          `Start_element (("" , "rt") , []) ;
                          `Text [b] ;
                          `End_element ;
                          `End_element ;
                         ]
                      | Tag (name , attrs , children) ->
                         let result = List.map transform children in
                         let result = [`Start_element (name , attrs)] :: result in
                         let result = List.append result [[`End_element]] in
                         List.concat result
                    in
                    queue := transform (Tag (("" , "p") , attrs , parsed));
                    pop ()
                 (* Anything else we pass through *)
                 | Some _ -> (); signal
            )
        in
        let result = M.write_xml (M.xhtml stream) in
        let result = M.to_string result in
        Z.add_entry result output entry.filename
      )
      spine_files
  in
  Z.close_out output
