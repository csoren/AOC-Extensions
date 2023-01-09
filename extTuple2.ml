open Batteries
include Tuple2

let bin_op op x y =
  (op (fst x) (fst y), op (snd x) (snd y))

let sub = bin_op Int.sub

let add = bin_op Int.add

let to_string t1 t2 (f, s) =
  Printf.sprintf "(%s,%s)" (t1 f) (t2 s) 
