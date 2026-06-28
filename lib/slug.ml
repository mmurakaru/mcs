(* Deterministic slugification for stable ids: lowercase ASCII, runs of
   non-alphanumerics collapse to a single hyphen, no leading/trailing hyphen. *)

let slugify (input : string) : string =
  let buffer = Buffer.create (String.length input) in
  let previous_was_hyphen = ref true (* true at the start suppresses a leading hyphen *) in
  String.iter
    (fun character ->
      match character with
      | 'A' .. 'Z' ->
        Buffer.add_char buffer (Char.lowercase_ascii character);
        previous_was_hyphen := false
      | 'a' .. 'z' | '0' .. '9' ->
        Buffer.add_char buffer character;
        previous_was_hyphen := false
      | _ ->
        if not !previous_was_hyphen then (
          Buffer.add_char buffer '-';
          previous_was_hyphen := true))
    input;
  let result = Buffer.contents buffer in
  let length = String.length result in
  if length > 0 && result.[length - 1] = '-' then String.sub result 0 (length - 1)
  else result
