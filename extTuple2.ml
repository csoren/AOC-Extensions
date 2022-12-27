open Batteries
include Tuple2

let bin_op op x y =
  (op (fst x) (fst y), op (snd x) (snd y))

let sub = bin_op Int.sub

let add = bin_op Int.add


