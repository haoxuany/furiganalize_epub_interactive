(* For reasons due to who knows why, there isn't a simple implode/explode implementation in BatUTF8,
which makes poping and pushing characters to string more complicated and much slower than its supposed to be, so we add them here *)
(* all of this is written to make sure that they actually run in O(n) *)
include BatUTF8

let explode (s : t) : BatUChar.t list =
  let result = ref [] in
  let () = iter (fun c -> result := c :: !result) s
  in
  List.rev (!result)

(* This runs in O(n), otherwise using iter is going force this into O(n ^ 2) *)
let implode (l : BatUChar.t list) : t =
  let l = List.map of_char l in
  String.concat "" l
