open Batteries

include List

let to_string conv list =
  let elements = map conv list |> String.concat "; " in
  "[" ^ elements ^ "]"
  
let string_list_to_string = to_string (fun s -> "\"" ^ s ^ "\"")

let int_list_to_string = to_string string_of_int

let int_list_list_to_string = to_string int_list_to_string

let bool_list_to_string = List.map (Bool.to_int) %> int_list_to_string

let bool_list_list_to_string = to_string bool_list_to_string

let group_at ~separator:sep l =
  List.nsplit sep l |> List.filter (not % List.is_empty)

let split_match p list =
  List.fold_while (fun _ el -> not @@ p el) (fun acc el -> el :: acc) [] list
  |> fun (first, second) -> (List.rev first, List.tl second)

let heads l = List.map List.hd l

let tails l = List.map List.tl l

let rec transpose = function
  | [] :: _ -> []
  | l -> heads l :: (tails l |> transpose)

let rdrop n = List.rev %> List.drop n %> List.rev 

let grade f init list =
  let step (acc, x) el =
    let y = f x el in
    (y :: acc, y)
  in
  fold_left step ([], init) list |> fst |> rev

let window_quadruplets list =
  match list with
  | a :: b :: c :: d :: _ ->
      let init = (a,b,c,d) in
      init :: grade (fun (_,b,c,d) e -> (b,c,d,e)) init list
  | _ -> []
  
let window_triplets list =
  grade (fun (_,b,c) d -> (b,c,d)) (0,0,0) list
  |> drop 2
    
let window_tuplets list =
  grade (fun (_,b) c -> (b,c)) (0,0) list
  |> drop 1

let window n list =
  let init = List.take n list in
  init :: grade (fun l e -> List.append (List.tl l) [e]) init list
 
  