(* Minimal, dependency-free JSON: a recursive-descent parser and a serializer.
   Scope is deliberately small - mcs only reads its own config and the index.json
   it generates - but it is a correct, escaping-safe implementation so generated
   files diff cleanly and round-trip. *)

type t =
  | Null
  | Boolean of bool
  | Number of float
  | String of string
  | List of t list
  | Object of (string * t) list

exception Parse_error of string

(* --- Serialize --- *)

let escape_string buffer text =
  Buffer.add_char buffer '"';
  Stdlib.String.iter
    (fun character ->
      match character with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | control when Char.code control < 0x20 ->
        Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code control))
      | other -> Buffer.add_char buffer other)
    text;
  Buffer.add_char buffer '"'

let number_to_string number =
  if Float.is_integer number && Float.abs number < 1e16 then Printf.sprintf "%.0f" number
  else Printf.sprintf "%.17g" number

let to_string ?(pretty = true) (json : t) : string =
  let buffer = Buffer.create 256 in
  let indent depth =
    if pretty then Buffer.add_string buffer (Stdlib.String.make (depth * 2) ' ')
  in
  let newline () = if pretty then Buffer.add_char buffer '\n' in
  let rec emit depth = function
    | Null -> Buffer.add_string buffer "null"
    | Boolean boolean -> Buffer.add_string buffer (if boolean then "true" else "false")
    | Number number -> Buffer.add_string buffer (number_to_string number)
    | String text -> escape_string buffer text
    | List [] -> Buffer.add_string buffer "[]"
    | List items ->
      Buffer.add_char buffer '[';
      newline ();
      List.iteri
        (fun position item ->
          if position > 0 then (
            Buffer.add_char buffer ',';
            newline ());
          indent (depth + 1);
          emit (depth + 1) item)
        items;
      newline ();
      indent depth;
      Buffer.add_char buffer ']'
    | Object [] -> Buffer.add_string buffer "{}"
    | Object fields ->
      Buffer.add_char buffer '{';
      newline ();
      List.iteri
        (fun position (key, value) ->
          if position > 0 then (
            Buffer.add_char buffer ',';
            newline ());
          indent (depth + 1);
          escape_string buffer key;
          Buffer.add_string buffer (if pretty then ": " else ":");
          emit (depth + 1) value)
        fields;
      newline ();
      indent depth;
      Buffer.add_char buffer '}'
  in
  emit 0 json;
  Buffer.contents buffer

(* --- Parse --- *)

let of_string (input : string) : t =
  let length = Stdlib.String.length input in
  let position = ref 0 in
  let peek () = if !position < length then Some input.[!position] else None in
  let fail message =
    raise (Parse_error (Printf.sprintf "%s at offset %d" message !position))
  in
  let rec skip_whitespace () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
      incr position;
      skip_whitespace ()
    | _ -> ()
  in
  let expect character =
    match peek () with
    | Some found when found = character -> incr position
    | _ -> fail (Printf.sprintf "expected '%c'" character)
  in
  let parse_literal literal value =
    let literal_length = Stdlib.String.length literal in
    if
      !position + literal_length <= length
      && Stdlib.String.sub input !position literal_length = literal
    then (
      position := !position + literal_length;
      value)
    else fail (Printf.sprintf "expected '%s'" literal)
  in
  let parse_string () =
    expect '"';
    let buffer = Buffer.create 16 in
    let rec loop () =
      match peek () with
      | None -> fail "unterminated string"
      | Some '"' -> incr position
      | Some '\\' -> (
        incr position;
        (match peek () with
        | Some '"' -> Buffer.add_char buffer '"'
        | Some '\\' -> Buffer.add_char buffer '\\'
        | Some '/' -> Buffer.add_char buffer '/'
        | Some 'n' -> Buffer.add_char buffer '\n'
        | Some 'r' -> Buffer.add_char buffer '\r'
        | Some 't' -> Buffer.add_char buffer '\t'
        | Some 'b' -> Buffer.add_char buffer '\b'
        | Some 'f' -> Buffer.add_char buffer '\012'
        | Some 'u' ->
          if !position + 4 >= length then fail "bad \\u escape";
          let hexadecimal = Stdlib.String.sub input (!position + 1) 4 in
          let code_point = int_of_string ("0x" ^ hexadecimal) in
          position := !position + 4;
          (* UTF-8 encode the basic-multilingual-plane code point *)
          if code_point < 0x80 then Buffer.add_char buffer (Char.chr code_point)
          else if code_point < 0x800 then (
            Buffer.add_char buffer (Char.chr (0xC0 lor (code_point lsr 6)));
            Buffer.add_char buffer (Char.chr (0x80 lor (code_point land 0x3F))))
          else (
            Buffer.add_char buffer (Char.chr (0xE0 lor (code_point lsr 12)));
            Buffer.add_char buffer (Char.chr (0x80 lor ((code_point lsr 6) land 0x3F)));
            Buffer.add_char buffer (Char.chr (0x80 lor (code_point land 0x3F))))
        | _ -> fail "bad escape");
        incr position;
        loop ())
      | Some character ->
        Buffer.add_char buffer character;
        incr position;
        loop ()
    in
    loop ();
    Buffer.contents buffer
  in
  let parse_number () =
    let start = !position in
    let is_number_character = function
      | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
      | _ -> false
    in
    while !position < length && is_number_character input.[!position] do
      incr position
    done;
    let token = Stdlib.String.sub input start (!position - start) in
    match float_of_string_opt token with Some number -> number | None -> fail "bad number"
  in
  let rec parse_value () =
    skip_whitespace ();
    match peek () with
    | None -> fail "unexpected end of input"
    | Some '{' -> parse_object ()
    | Some '[' -> parse_array ()
    | Some '"' -> String (parse_string ())
    | Some 't' -> parse_literal "true" (Boolean true)
    | Some 'f' -> parse_literal "false" (Boolean false)
    | Some 'n' -> parse_literal "null" Null
    | Some ('-' | '0' .. '9') -> Number (parse_number ())
    | Some _ -> fail "unexpected character"
  and parse_object () =
    expect '{';
    skip_whitespace ();
    match peek () with
    | Some '}' ->
      incr position;
      Object []
    | _ ->
      let rec loop accumulator =
        skip_whitespace ();
        let key = parse_string () in
        skip_whitespace ();
        expect ':';
        let value = parse_value () in
        skip_whitespace ();
        match peek () with
        | Some ',' ->
          incr position;
          loop ((key, value) :: accumulator)
        | Some '}' ->
          incr position;
          Object (List.rev ((key, value) :: accumulator))
        | _ -> fail "expected ',' or '}'"
      in
      loop []
  and parse_array () =
    expect '[';
    skip_whitespace ();
    match peek () with
    | Some ']' ->
      incr position;
      List []
    | _ ->
      let rec loop accumulator =
        let value = parse_value () in
        skip_whitespace ();
        match peek () with
        | Some ',' ->
          incr position;
          loop (value :: accumulator)
        | Some ']' ->
          incr position;
          List (List.rev (value :: accumulator))
        | _ -> fail "expected ',' or ']'"
      in
      loop []
  in
  let value = parse_value () in
  skip_whitespace ();
  if !position <> length then fail "trailing data";
  value

(* --- Accessors --- *)

let member key = function Object fields -> List.assoc_opt key fields | _ -> None

let to_string_opt = function String text -> Some text | _ -> None

let to_list_opt = function List items -> Some items | _ -> None

let string_list = function
  | List items -> List.filter_map to_string_opt items
  | _ -> []
