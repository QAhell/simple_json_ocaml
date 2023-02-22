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

module Json_printer :
  functor (Output : sig include Utf8_stream.Code_point_output val put_str : t -> string -> t end) ->
    sig
      exception Invalid_json_number
      exception Invalid_code_point
      val print_null : Output.t -> Output.t
      val print_boolean : Output.t -> bool -> Output.t
      val print_number : Output.t -> json_number -> Output.t
      val print_string : Output.t -> string -> Output.t
      val print_array : Output.t -> json_array -> Output.t
      val print_object : Output.t -> json_object -> Output.t
      val print_json : Output.t -> json_value -> Output.t
    end
