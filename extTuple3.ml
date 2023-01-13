open Batteries
include Tuple3

let bin_op op x y=
  (op (Tuple3.first x) (Tuple3.first y), op (Tuple3.second x) (Tuple3.second y), op (Tuple3.third x) (Tuple3.third y))

let sub = bin_op Int.sub

let add = bin_op Int.add

let to_string (x, y, z) =
  Printf.sprintf "(%d,%d,%d)" x y z

