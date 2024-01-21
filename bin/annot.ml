open! Batteries

type 'a cached =
  'a option ref
    [@@deriving show]

let from_cache (a : 'a cached) (f : unit -> 'a) =
  match !a with
  | None -> 
     let result = f () in
     a := Some result;
     result
  | Some s -> s

type unicode_string =
  string [@printer Format.pp_print_string]
    [@@deriving show]

type word =
  { segs : (unicode_string * unicode_string option) list
  ; base : unicode_string cached
  ; annot : unicode_string cached
  }
    [@@deriving show]

let to_string ({ segs ; _ }) : string =
  List.map
    (fun (a , b) ->
      match b with
      | None -> a
      | Some b ->
         String.concat ""
           [ "[" ; a ; "]" ; "(" ; b ; ")"]
    ) segs
  |> String.concat ""

let of_segs segs = { segs ; base = ref None ; annot = ref None }

let of_string s =
  let s = String.explode s in
  let rec parse_base s result =
    match s with
    | ']' :: rest -> (String.implode (List.rev result) , rest)
    | x :: rest -> parse_base rest (x :: result)
    | [] -> (String.implode (List.rev result) , [])
  in
  let rec parse_annot s result =
    match s with
    | ')' :: rest -> (String.implode (List.rev result) , rest)
    | x :: rest -> parse_annot rest (x :: result)
    | [] -> (String.implode (List.rev result) , [])
  in
  let rec parse (s : char list) (result : (string * string option) list) =
    let push_raw (c : string) =
      match result with
      | [] | (_ , Some _) :: _ ->
         (c , None) :: result
      | (a , None) :: rest ->
         (String.concat "" [a ; c] , None) :: rest
    in
    match s with
    | '[' :: rest ->
       begin
         let (base , rest) = parse_base rest [] in
         match rest with
         | [] -> parse [] ((base , None) :: result)
         | '(' :: rest ->
            let (annot , rest) = parse_annot rest [] in
            parse rest ((base , Some annot) :: result)
         | x :: rest -> 
            begin
              let result =
                push_raw
                  (String.concat "" ["[" ; base ; "]" ; String.implode [x]])
              in parse rest result
            end
       end
    | x :: rest ->
       let result = push_raw (String.implode [x]) in
       parse rest result
    | [] -> List.rev result
  in
  let segs = parse s []
  in of_segs segs

let segs ({ segs ; _ }) = segs

let base ({ segs ; base ; _ }) : string =
  from_cache base 
    (fun () -> String.concat "" (List.map (fun (a , _) -> a) segs))

let annot ({ segs ; annot ; _ }) : string =
  from_cache annot 
    (fun () ->
      String.concat ""
        (List.map
           (fun (_ , b) ->
             match b with
             | None -> "?"
             | Some s -> s
           ) segs)
    )

let eq_base a b : bool = String.equal (base a) (base b)
