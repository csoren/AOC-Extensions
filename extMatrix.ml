open Batteries

type 'a t = 'a array array

let make columns rows init =
  Array.make_matrix columns rows init

let is_empty = ExtArray.is_empty

let width (m: 'a t) = Array.length m

let column n (m: 'a t) =
  Array.get m n |> Array.to_list

let columns (m: 'a t) =
  Array.to_list m |> List.map Array.to_list

let height (m: 'a t) =
  Array.get m 0 |> Array.length 

let row n (m: 'a t) =
  Array.fold_left (fun acc column -> column.(n) :: acc) [] m
  |> List.rev

let transpose (m: 'a t) =
  if is_empty m then
    [||]
  else
    List.range 0 `To (height m - 1)
    |> List.map (Fun.flip row m %> Array.of_list)
    |> Array.of_list

let rows (m: 'a t) =
  transpose m |> columns

let of_columns cols =
  List.map Array.of_list cols |> Array.of_list
    
let of_rows rows =
  of_columns rows |> transpose

let filter_rows fn (m: 'a t) =
  rows m |> List.filter fn |> of_rows

let map_columns fn (m: 'a t) =
  columns m |> List.map fn
  
let mapi_columns fn (m: 'a t) =
  columns m |> List.mapi fn
  
let map_rows fn (m: 'a t) =
  rows m |> List.map fn

let mapi_rows fn (m: 'a t) =
  rows m |> List.mapi fn

let to_string m =
  rows m |> ExtList.bool_list_list_to_string

let clamp_row_indices m n1 n2 =
  let clamp n = ExtInt.clamp n 0 (width m - 1) in
  (clamp n1, clamp n2)

let clamp_column_indices m n1 n2 =
  let clamp n = ExtInt.clamp n 0 (height m - 1) in
  (clamp n1, clamp n2)

let row_range m n1 n2 =
  let (n1', n2') = clamp_row_indices m n1 n2 in
  ExtList.map_range n1' n2'
  
let column_range m n1 n2 =
  let (n1', n2') = clamp_column_indices m n1 n2 in
  ExtList.map_range n1' n2'
  
let sub_row_right (m: 'a t) x y n =
  row_range m x (x + n - 1)
  |> List.map (fun x -> m.(x).(y))

let sub_row_right_edge (m: 'a t) x y =
  sub_row_right m x y (width m - x)

let sub_row_left (m: 'a t) x y n =
  row_range m x (x - n + 1)
  |> List.map (fun x -> m.(x).(y))

let sub_row_left_edge (m: 'a t) x y =
  sub_row_left m x y (x + 1)

let sub_column_down (m: 'a t) x y n =
  column_range m y (y + n - 1)
  |> List.map (fun y -> m.(x).(y))

let sub_column_bottom_edge (m: 'a t) x y =
  sub_column_down m x y (height m - y)

let sub_column_up (m: 'a t) x y n =
  column_range m y (y - n + 1)
  |> List.map (fun y -> m.(x).(y))

let sub_column_top_edge (m: 'a t) x y =
  sub_column_up m x y (y + 1)

let coords m =
  let coli = List.range 0 `To (width m - 1) in
  let rowi = List.range 0 `To (height m - 1) in
  List.map (fun row -> List.map (fun col -> (col, row)) coli) rowi |> List.flatten

let findi_opt f m =
  coords m |> List.find_opt (fun (col, row) -> f col row m.(col).(row))

let copy m =
  Array.map Array.copy m

let coord_valid x y m =
  x >= 0 && x < width m && y >= 0 && y < height m 
