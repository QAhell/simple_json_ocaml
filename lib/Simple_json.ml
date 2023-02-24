open Simple_parser_combinator
module String_map = Map.Make(String)

type json =
  | Json of json_value
and json_value =
  | Object of json_object
  | Array of json_array
  | String of string
  | Number of json_number
  | Boolean of bool
  | Null
and json_object = json_value String_map.t
and json_array = json_value list
and json_number = json_sign * string * string * (json_sign * string) option
and json_sign = Positive | Negative

module type Json_constructors =
sig
  type digits
  type text
  type value
  type arr
  type obj
  val of_null : value
  val of_bool : bool -> value
  val of_number : json_sign -> digits -> digits -> (json_sign * digits) option -> value
  val of_text : text -> value
  val of_array : arr -> value
  val of_object : obj -> value
  val empty_digits : digits
  val put_digit : digits -> char -> digits
  val empty_text : text
  val put_code_point : text -> int -> text
  val empty_array : arr
  val put_array : arr -> value -> arr
  val empty_object : obj
  val put_key_value : obj -> text -> value -> obj
end

module Default_json_constructors =
struct
  type digits = Utf8_stream.Encoded_string_output.t
  type text = Utf8_stream.Encoded_string_output.t
  type value = json_value
  type arr = json_array
  type obj = json_object
  let of_null = Null
  let of_bool b = Boolean b
  let of_number sign integer fraction exponent =
    Number (sign, Utf8_stream.Encoded_string_output.to_string integer,
      Utf8_stream.Encoded_string_output.to_string fraction,
      match exponent with
        | None -> None
        | Some (sign, exponent) -> Some (sign, Utf8_stream.Encoded_string_output.to_string exponent))
  let of_text text = String (Utf8_stream.Encoded_string_output.to_string text)
  let of_array arr = Array (List.rev arr)
  let of_object obj = Object obj
  let empty_digits = Utf8_stream.Encoded_string_output.empty ()
  let empty_text = Utf8_stream.Encoded_string_output.empty ()
  let empty_object = String_map.empty
  let empty_array = []
  let put_array xs x = x :: xs
  let put_digit ds d = Utf8_stream.Encoded_string_output.put ds (int_of_char d)
  let put_code_point = Utf8_stream.Encoded_string_output.put
  let put_key_value m key value =
    String_map.add (Utf8_stream.Encoded_string_output.to_string key) value m
end

module Json_parser =
  functor (Json_constructors : Json_constructors) ->
  functor (Parser : Parser_combinator) ->
  functor (Input : Utf8_stream.Code_point_input) ->
  functor (Make_error : sig val make_error : string -> Input.t -> Parser.Error_info.t end) ->
    struct
      open Parser

      module C = Code_point_parsers(Input)(Make_error)
      open C

      let parse_null = convert (expect_string "null") (fun _ -> Json_constructors.of_null)

      let parse_boolean =
        convert (or_else (expect_string "true") (expect_string "false"))
          (function "true" -> Json_constructors.of_bool true | "false" -> Json_constructors.of_bool false
              | _ -> raise (Failure "Parser combinator implementation error! Boolean should either be true or false!"))

      let parse_onenine =
          expect "onenine" (fun input ->
            match Input.get input with
              | Some (digit, input)
                    when int_of_char '1' <= digit && digit <= int_of_char '9' ->
                  Either.Left (char_of_int (digit), input)
              | Some (other, input) ->
                  Either.Right (Make_error.make_error (
                      "Expected digit 1-9, found code point " ^ string_of_int other) input)
              | None ->
                  Either.Right (Make_error.make_error (
                      "Expected digit 1-9, found end of input") input))

      let parse_digit =
          expect "digit" (fun input ->
            match Input.get input with
              | Some (digit, input)
                    when int_of_char '0' <= digit && digit <= int_of_char '9' ->
                  Either.Left (char_of_int (digit), input)
              | Some (other, input) ->
                  Either.Right (Make_error.make_error (
                      "Expected digit, found code point " ^ string_of_int other) input)
              | None ->
                  Either.Right (Make_error.make_error (
                      "Expected digit, found end of input") input))

      let parse_digits at_least_one acc =
        let repeat = if at_least_one then once_or_more_and_fold_left else repeat_and_fold_left in
        repeat parse_digit acc Json_constructors.put_digit

      let parse_integer =
        and_then (convert (optional (expect_string "-")) (function Some _ -> Negative | _ -> Positive))
          (or_else (bind parse_onenine (fun d ->
                      let acc = Json_constructors.put_digit Json_constructors.empty_digits d in
                      parse_digits false acc))
                  (convert parse_digit (Json_constructors.put_digit Json_constructors.empty_digits)))

      let parse_fraction =
        convert (optional (
              and_then (expect_string ".")
                  (parse_digits true Json_constructors.empty_digits)))
          (function Some (_, s) -> s | None -> Json_constructors.empty_digits)

      let parse_sign =
        convert (optional (or_else (expect_string "+") (expect_string "-")))
          (function Some "-" -> Negative
            | _ -> Positive)

      let parse_exponent =
        optional
          (second (and_then (or_else (expect_string "E") (expect_string "e"))
                   (and_then parse_sign (parse_digits true Json_constructors.empty_digits))))

      let parse_number =
        convert (and_then parse_integer (and_then parse_fraction parse_exponent))
          (fun ((sign, i), (f, e)) -> Json_constructors.of_number sign i f e)

      (*let escaped_quote = int_of_char '"'       (*let escaped_quote = 34
      let escaped_backslash = int_of_char '\\'    let escaped_backslash = 92
      let escaped_slash = int_of_char '/'         let escaped_slash = 47
      let escaped_beep = int_of_char 'b'          let escaped_beep = 98
      let escaped_page_forward = int_of_char 'f'  let escaped_page_forward = 102
      let escaped_newline = int_of_char 'n'       let escaped_newline = 110
      let escaped_linefeed = int_of_char 'r'      let escaped_linefeed = 114
      let escaped_tab = int_of_char 't'           let escaped_tab = 116
      let escaped_hex = int_of_char 'u'*)         let escaped_hex = 117*)

      let parse_escape = (* without leading backslash *)
        expect "parse_escape" (fun input ->
          match Input.get input with
            | (* escaped_quote *) Some (34, input) -> Either.Left (int_of_char '"', input)
            | (* escaped_backslash *) Some (92, input) -> Either.Left (int_of_char '\\', input)
            | (* escaped_slash *) Some (47, input) -> Either.Left (int_of_char '/', input)
            | (* escaped_beep *) Some (98, input) -> Either.Left (int_of_char '\b', input)
            | (* escaped_page_forward *) Some (102, input) -> Either.Left (12, input)
            | (* escaped_newline *) Some (110, input) -> Either.Left (int_of_char '\n', input)
            | (* escaped_linefeed *) Some (114, input) -> Either.Left (int_of_char '\r', input)
            | (* escaped_tab *) Some (116, input) -> Either.Left (int_of_char '\t', input)
            | (* escaped_hex *) Some (117, input) ->
              let is_hex_digit d =
                int_of_char '0' <= d && d <= int_of_char '9' ||
                int_of_char 'a' <= d && d <= int_of_char 'f' ||
                int_of_char 'A' <= d && d <= int_of_char 'F' in
              let error_at_char other input =
                Either.Right (Make_error.make_error (
                  "Expected hex digit, found code point " ^ string_of_int other) input) in
              let error_at_end input =
                Either.Right (Make_error.make_error (
                  "Expected hex digit, found end of input") input) in
              (match Input.get input with Some (c0, input) when is_hex_digit c0 -> 
              (match Input.get input with Some (c1, input) when is_hex_digit c1 -> 
              (match Input.get input with Some (c2, input) when is_hex_digit c2 -> 
              (match Input.get input with Some (c3, input) when is_hex_digit c3 -> 
                  let convert d =
                    if int_of_char '0' <= d && d <= int_of_char '9' then d - int_of_char '0' else
                    if int_of_char 'a' <= d && d <= int_of_char 'f' then d - int_of_char 'a' + 10 else
                    d - int_of_char 'A' + 10 in
                  let u0 = convert c0 in
                  let u1 = convert c1 in
                  let u2 = convert c2 in
                  let u3 = convert c3 in
                    Either.Left ((((u0 * 0x10) + u1) * 0x10 + u2) * 0x10 + u3, input)
                | Some (other, input) -> error_at_char other input | None -> error_at_end input)
                | Some (other, input) -> error_at_char other input | None -> error_at_end input)
                | Some (other, input) -> error_at_char other input | None -> error_at_end input)
                | Some (other, input) -> error_at_char other input | None -> error_at_end input)
            | Some (wrong, input) ->
                Either.Right (Make_error.make_error (
                      "Expected escape sequence, found code point " ^ string_of_int wrong) input)
            | None ->
                Either.Right (Make_error.make_error (
                      "Expected escape sequence, found end of input") input))

      let parse_non_escape_character =
        expect "parse_non_escape_character" (fun input ->
          match Input.get input with
            | Some (c, input) when 0x20 <= c && c <= 0x10FFFF && c <> int_of_char '"' && c <> int_of_char '\\' ->
                Either.Left (c, input)
            | Some (c, input) ->
                Either.Right (Make_error.make_error (
                      "Expected in-string character, found code point " ^ string_of_int c) input)
            | None ->
                Either.Right (Make_error.make_error (
                      "Expected in-string character, found end of input") input))

      let parse_character =
        or_else parse_non_escape_character
          (second (and_then (expect_code_point (int_of_char '\\')) parse_escape))

      let parse_characters acc =
        repeat_and_fold_left parse_character acc Json_constructors.put_code_point

      let parse_string_raw =
        let parse_quote = expect_code_point (int_of_char '"') in
        convert (and_then
          parse_quote
          (and_then
            (parse_characters Json_constructors.empty_text)
            parse_quote)) 
          (fun (_, (acc, _)) -> acc)

      let parse_string =
        convert parse_string_raw (fun s -> Json_constructors.of_text s)

      let parse_atom =
        optional_spaces_before (
            or_else parse_null
           (or_else parse_boolean
           (or_else parse_string
              parse_number)))

      let parse_value =
        recurse (fun parse_value ->
          let parse_array =
            second (and_then (optional_spaces_before (expect_code_point (int_of_char '[')))
             (first (and_then
                (convert
                 (optional
                   (bind parse_value (fun x ->
                      (repeat_and_fold_left
                       (second
                        (and_then (optional_spaces_before (expect_code_point (int_of_char ','))) parse_value))
                       (Json_constructors.put_array Json_constructors.empty_array x)
                       (fun xs x -> Json_constructors.put_array xs x)))))
                (function None -> Json_constructors.of_array Json_constructors.empty_array
                    | Some xs -> Json_constructors.of_array xs))
                (optional_spaces_before (expect_code_point (int_of_char ']')))))) in
          let parse_member =
            and_then (optional_spaces_before parse_string_raw)
              (second (and_then (optional_spaces_before (expect_code_point (int_of_char ':'))) parse_value)) in
          let parse_members =
            convert
              (bind parse_member (fun (k0, v0) ->
                (repeat_and_fold_left (second (and_then
                    (optional_spaces_before (expect_code_point (int_of_char ',')))
                    parse_member))
                  (Json_constructors.put_key_value Json_constructors.empty_object k0 v0)
                  (fun obj (k, v) -> Json_constructors.put_key_value obj k v))))
              Json_constructors.of_object in
          let parse_object =
            second (and_then (optional_spaces_before (expect_code_point (int_of_char '{')))
              (first (and_then
                 (convert (optional parse_members)
                    (function Some x -> x
                       | None -> Json_constructors.of_object Json_constructors.empty_object))
                (optional_spaces_before (expect_code_point (int_of_char '}')))))) in
          or_else parse_atom (or_else parse_array parse_object))

      let parse_json = parse_value

    end

module Default_json_parser =
  functor (Parser : Parser_combinator) ->
  functor (Input : Utf8_stream.Code_point_input) ->
  functor (Make_error : sig val make_error : string -> Input.t -> Parser.Error_info.t end) ->
    Json_parser(Default_json_constructors)(Parser)(Input)(Make_error)

module Json_printer =
  functor (Output : sig include Utf8_stream.Code_point_output val put_str : t -> string -> t end) ->
    struct
      exception Invalid_json_number
      exception Invalid_code_point
      let print_null o = Output.put_str o "null"
      let print_boolean o = function
        | true -> Output.put_str o "true"
        | false -> Output.put_str o "false"
      let print_number o (s, i, f, e) =
        let print_sign o = function
          | Positive -> o
          | Negative -> Output.put_str o "-" in
        let assert_digits s =
          String.iter (fun c ->
            if c < '0' || c > '9' then
              begin
                raise Invalid_json_number
              end) s in
        let print_digits o digits =
          assert_digits digits ;
          Output.put_str o digits in
        let print_nonempty_digits o digits =
          if i = "" then
            begin
              raise Invalid_json_number
            end ;
          print_digits o digits in
        let o = print_sign o s in
        let o =
          if i <> "0" && String.starts_with ~prefix:"0" i then
            begin
              raise Invalid_json_number
            end ;
          print_nonempty_digits o i in
        let o = if f = "" then o else print_digits (Output.put_str o ".") f in
        match e with
          | None -> o
          | Some (s, e) ->
            let o = Output.put o (int_of_char 'e') in
            let o = print_sign o s in
            let o = print_nonempty_digits o e in
            o

      let print_code_point_in_string out code_point =
        if code_point < 0 || code_point > 0x10FFFF then
          raise Invalid_code_point
        else if code_point = int_of_char '"' then
          Output.put_str out "\\\""
        else if code_point = int_of_char '\\' then
          Output.put_str out "\\\\"
        else if code_point < 0x20 then
          let escaped_char = Printf.sprintf "\\u00%02X" code_point in
          Output.put_str out escaped_char
        else
          Output.put out code_point

      let print_string out text =
        let out = Output.put out (int_of_char '"') in
        let input = Utf8_stream.Decoded_string_input.of_string text in
        let rec walk_input out input =
          match Utf8_stream.Decoded_string_input.get input with
            | Some (code_point, input) -> walk_input (print_code_point_in_string out code_point) input
            | None -> out in
        let out = walk_input out input in
        let out = Output.put out (int_of_char '"') in
        out

      let rec print_array out arr =
        let out = Output.put out (int_of_char '[') in
        let rec walk_array is_first out = function
          | obj :: arr ->
            let out = if is_first then out else Output.put out (int_of_char ',') in
            let out = print_json out obj in
            walk_array false out arr
          | [] -> out in
        let out = walk_array true out arr in
        let out = Output.put out (int_of_char ']') in
        out

      and print_json out = function
        | Null -> print_null out
        | Boolean b -> print_boolean out b
        | Number n -> print_number out n
        | String s -> print_string out s
        | Array a -> print_array out a
        | Object o -> print_object out o

      and print_object out obj =
        let out = Output.put out (int_of_char '{') in
        let walk_obj key value (is_first, out) =
          (false,
           let out = if is_first then out else Output.put out (int_of_char ',') in
           let out = print_string out key in
           let out = Output.put out (int_of_char ':') in
           let out = print_json out value in
           out) in
        let (_, out) = String_map.fold walk_obj obj (true, out) in
        let out = Output.put out (int_of_char '}') in
        out
    end
