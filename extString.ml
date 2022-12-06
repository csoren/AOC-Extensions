open Batteries

include String

let drop n s =
  String.sub s n (String.length s - n)

let take_while f s =
  String.to_list s |> List.take_while f |> String.of_list

let drop_while f s =
  String.to_list s |> List.drop_while f |> String.of_list

let ntake n s =
  String.to_list s |> List.ntake n |> List.map String.of_list
 
let hd s =
  String.get s 0

let split_on_char_greedy ch =
  String.split_on_char ch %> List.filter (String.is_empty %> Bool.not)
