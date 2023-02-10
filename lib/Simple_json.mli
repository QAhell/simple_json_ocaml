open Simple_parser_combinator
module String_map : Map.S with type key = string

(** Non-streaming JSON parser. Use this parser only if your JSON fits in main memory. *)

type json =
  | Json of json_value
and json_value =
  | Object of json_object
  | Array of json_array
  | String of string
    (** All escaped characters have been unescaped including \uXXXX.
      The string is utf-8 encoded. The string only contains the content
      and not the double quotes from the JSON syntax. *)

  | Number of json_number
  | Boolean of bool
  | Null
and json_object = json_value String_map.t
and json_array = json_value list

and json_number = json_sign * string * string * (json_sign * string) option
(** Sign, integer part, fraction, exponent sign, exponent value.
  One of integer or fraction part may be an empty string. *)

and json_sign = Positive | Negative


module Json_parser :
  functor (Parser : Parser_combinator) ->
  functor (Input : Utf8_stream.Code_point_input) ->
  functor (_ : sig val make_error : string -> Input.t -> Parser.Error_info.t end) ->
    sig
      val parse_json : (Input.t, json_value) Parser.t

      val parse_null : (Input.t, json_value) Parser.t

      val parse_boolean : (Input.t, json_value) Parser.t

      val parse_number : (Input.t, json_value) Parser.t

      val parse_string : (Input.t, json_value) Parser.t
    end
