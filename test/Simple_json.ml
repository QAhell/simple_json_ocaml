open Simple_json
open Simple_parser_combinator

module E =
struct
  type t = string * int
  let default_error = ("<default>", -1)
  let is_default_error (_, n) = n < 0
  let is_real_error e = not (is_default_error e)
  let merge (e1, n1) (e2, n2) =
    if n1 >= n2 then (e1, n1)
    else (e2, n2)
  let make_error s (n, _) = (s, n)
end

module P = Recursive_descent_parser (Trampoline.Not_a_trampoline) (E)
module J = Json_parser (P) (Utf8_stream.Decoded_string_input) (E)

let assertTrue condition message =
  assert (condition || (print_endline message ; condition)) ;;

let prs p s =
  let input = Utf8_stream.Decoded_string_input.of_string s in
  P.execute p (fun output (n, input) error ->
    if String.length input <= n then Either.Left output else Either.Right error)
    (fun error -> Either.Right (Some error))
    input

let has_error f x =
  match f x with
    | Either.Right (Some _) -> true
    | _ -> false

let test_parse_null () =
  assertTrue (prs J.parse_null "null" = Either.Left Null)
    "The string \"null\" must be parsed as Null" ;
  assertTrue (match prs J.parse_null "nill" with Either.Right (Some _) -> true | _ -> false)
    "The string \"nill\" must NOT be parsed as Null" ;;

let test_parse_boolean () =
  assertTrue (prs J.parse_boolean "true" = Either.Left (Boolean true))
    "The parser must parse the string \"true\" as Boolean true" ;
  assertTrue (prs J.parse_boolean "false" = Either.Left (Boolean false))
    "The parser must parse the string \"false\" as Boolean false" ;
  assertTrue (has_error (prs J.parse_boolean) "True")
    "The parser must NOT parse the string \"True\" as Boolean" ;
  assertTrue (has_error (prs J.parse_boolean) "FALSE")
    "The parser must NOT parse the string \"FALSE\" as Boolean" ;;

let test_parse_number () =
  assertTrue (prs J.parse_number "123" = Either.Left (Number (Positive, "123", "", None)))
    "The parser must parse the string \"123\" as a number" ;
  assertTrue (prs J.parse_number "0" = Either.Left (Number (Positive, "0", "", None)))
    "The parser must parse the string \"0\" as a number" ;
  assertTrue (prs J.parse_number "0.0E+000" = Either.Left (Number (Positive, "0", "0", Some (Positive, "000"))))
    "The parser must parse the string \"0.0E+000\" as a number" ;
  assertTrue (prs J.parse_number "-123" = Either.Left (Number (Negative, "123", "", None)))
    "The parser must accept a negative minus sign" ;
  assertTrue (prs J.parse_number "0.123" = Either.Left (Number (Positive, "0", "123", None)))
    "The parser must parse the string \"0.123\" as a number" ;
  assertTrue (prs J.parse_number "-0.123" = Either.Left (Number (Negative, "0", "123", None)))
    "The parser must parse the string \"-0.123\" as a number" ;
  assertTrue (prs J.parse_number "1e10" = Either.Left (Number (Positive, "1", "", Some (Positive, "10"))))
    "The parser must parse the string \"1e10\" as a number" ;
  assertTrue (prs J.parse_number "1e+10" = Either.Left (Number (Positive, "1", "", Some (Positive, "10"))))
    "The parser must parse the string \"1e+10\" as a number" ;
  assertTrue (prs J.parse_number "1e-10" = Either.Left (Number (Positive, "1", "", Some (Negative, "10"))))
    "The parser must parse the string \"1e-10\" as a number" ;
  assertTrue (prs J.parse_number "-1e10" = Either.Left (Number (Negative, "1", "", Some (Positive, "10"))))
    "The parser must parse the string \"-1e10\" as a number" ;
  assertTrue (prs J.parse_number "-1e+10" = Either.Left (Number (Negative, "1", "", Some (Positive, "10"))))
    "The parser must parse the string \"-1e+10\" as a number" ;
  assertTrue (prs J.parse_number "-1e-10" = Either.Left (Number (Negative, "1", "", Some (Negative, "10"))))
    "The parser must parse the string \"-1e-10\" as a number" ;
  assertTrue (has_error (prs J.parse_number) "+123")
    "The parser must NOT accept a positive plus sign" ;
  assertTrue (has_error (prs J.parse_number) "+0.123")
    "The parser must NOT accept the string \"+0.123\"" ;
  assertTrue (has_error (prs J.parse_number) ".123")
    "The parser must NOT accept the string \".123\"" ;
  assertTrue (has_error (prs J.parse_number) "123.")
    "The parser must NOT accept the string \"123.\"" ;
  assertTrue (has_error (prs J.parse_number) "0123")
    "The parser must NOT accept the string \"0123\"" ;
  () ;;

let test_parse_string () =
  assertTrue (prs J.parse_string "\"\"" = Either.Left (String ""))
    "The parser must parse the empty string \"\"" ;
  assertTrue (prs J.parse_string "\"test 123\"" = Either.Left (String "test 123"))
    "The parser must parse the string \"test 123\"" ;
  assertTrue (prs J.parse_string "\"美酒加咖啡\"" = Either.Left (String "美酒加咖啡"))
    "The parser must parse the string \"美酒加咖啡\"" ;
  assertTrue (prs J.parse_string "\"\\\\\\/\\\"\\b\\f\\n\\r\\t\"" = Either.Left (String "\\/\"\b\x0C\n\r\t"))
    "The parser must parse the string \"\\\\\\/\\\"\\b\\f\\n\\r\\t\"" ;
  assertTrue (prs J.parse_string "\"\xF4\x8F\xBF\xBF\"" = Either.Left (String "\xF4\x8F\xBF\xBF"))
    "The parser must parse the string \"\xF4\x8F\xBF\xBF\"" ;
  assertTrue (prs J.parse_string "\"\\u9F8d\"" = Either.Left (String "龍"))
    "The parser must parse the string \"\\u9F8D\"" ;
  assertTrue (has_error (prs J.parse_string) "\"\x05\"")
    "The parser must NOT accept \"\x05\"" ;
  assertTrue (has_error (prs J.parse_string) "'test'")
    "The parser must NOT accept 'test'" ;
  assertTrue (has_error (prs J.parse_string) "\"test")
    "The parser must NOT accept \"test" ;
  () ;;

let test_parse_object () =
  assertTrue (match prs J.parse_json "{}" with Either.Left (Object o) -> (String_map.is_empty o) | _ -> false)
    "The parser must parse {} as the empty object" ;
  assertTrue (match prs J.parse_json "  {  }" with Either.Left (Object o) -> (String_map.is_empty o) | _ -> false)
    "The parser must parse \"  {  }\" as the empty object" ;
  assertTrue (match prs J.parse_json "{\"a\": 1, \"b\": true}" with
      | Either.Left (Object o) ->
          String_map.cardinal o = 2 &&
          String_map.find "a" o = Number (Positive, "1", "", None) &&
          String_map.find "b" o = Boolean true
      | _ -> false)
    "The parse must parse {\"a\": 1, \"b\": true} as an object" ;
  assertTrue (match prs J.parse_json "{\"a\": 1, \"a\": 2}" with
      | Either.Left (Object o) ->
          String_map.cardinal o = 1 &&
          String_map.find "a" o = Number (Positive, "2", "", None)
      | _ -> false)
    "The parse must parse {\"a\": 1, \"b\": true} as an object" ;
  assertTrue (match prs J.parse_json "{\"o\": {}}" with
      | Either.Left (Object o) ->
          String_map.cardinal o = 1 &&
          (match String_map.find "o" o with
            | Object o -> String_map.is_empty o
            | _ -> false)
      | _ -> false)
    "The parse must parse {\"a\": 1, \"b\": true} as an object" ;
  assertTrue (has_error (prs J.parse_json) "{a: 1}")
    "The parser must NOT accept {a: 1}" ;
  assertTrue (has_error (prs J.parse_json) "{\"a\": 1; \"b\": true}")
    "The parser must NOT accept {\"a\": 1; \"b\": true}" ;
  assertTrue (has_error (prs J.parse_json) "{\"a\": 1, \"b\": true")
    "The parser must NOT accept {\"a\": 1, \"b\": true" ;
  () ;;

let test_parse_array () =
  assertTrue (match prs J.parse_json "[]" with Either.Left (Array []) -> true | _ -> false)
    "The parser must parse [] as the empty array" ;
  assertTrue (match prs J.parse_json "  [  ]" with Either.Left (Array []) -> true | _ -> false)
    "The parser must parse \"  [  ]\" as the empty array" ;
  assertTrue (match prs J.parse_json "[1,true,\"a\",{}]" with Either.Left (Array [Number (Positive, "1", "", None); Boolean true; String "a"; Object _]) -> true | _ -> false)
    "The parse must parse [1,true,\"a\",{}] as an array" ;
  assertTrue (has_error (prs J.parse_json) "[1,true,\"a\",{}")
    "The parse must NOT accept [1,true,\"a\",{}" ;
  () ;;
  

test_parse_null () ;;
test_parse_boolean () ;;
test_parse_number () ;;
test_parse_string () ;;
test_parse_object () ;;
test_parse_array () ;;
