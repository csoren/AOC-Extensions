open Batteries

include List

let to_string conv list =
  let elements = map conv list |> String.concat "; " in
  "[" ^ elements ^ "]"
  
let string_list_to_string = to_string (fun s -> "\"" ^ s ^ "\"")

let string_list_list_to_string = to_string string_list_to_string

let int_list_to_string = to_string string_of_int

let int_list_list_to_string = to_string int_list_to_string

let bool_list_to_string = List.map (Bool.to_int) %> int_list_to_string

let bool_list_list_to_string = to_string bool_list_to_string

let group_at ~separator:sep l =
  List.nsplit sep l |> List.filter (not % List.is_empty)

let group_when ~first:sep l =
  let add_when acc el =
    if sep el then
      [el] :: acc
    else
      (el :: List.hd acc) :: List.tl acc
  in
  List.fold_left add_when [] l
  |> List.map List.rev
  |> List.rev

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
 
let map_range start stop =
  if start < stop then
    List.range start `To stop
  else
    List.range start `Downto stop

let head_opt = function
  | [] -> None
  | h :: _ -> Some h

let min_opt cmp =
  let fold_fn acc el =
    match acc with
    | Some n -> Some (if cmp n el < 0 then n else el)
    | None -> Some el
  in
  List.fold fold_fn None

let max_opt cmp =
  let fold_fn acc el =
    match acc with
    | Some n -> Some (if cmp n el > 0 then n else el)
    | None -> Some el
  in
  List.fold fold_fn None

let findi_opt f l =
  let rec find' i = function
    | [] -> None
    | e :: _ when f e -> Some (i, e)
    | _ :: tail -> find' (i + 1) tail
  in
  find' 0 l
  
let flat_map fn = List.map fn %> List.flatten

let flatten_opt l =
  let flatten_opt' l = function
    | Some v -> v :: l
    | None -> l
  in
  List.fold flatten_opt' [] l |> List.rev

let flat_map_opt fn = List.map fn %> flatten_opt
