open Batteries

include Array

let get_set f a n =
  Array.set a n (f (Array.get a n))

let is_empty a =
  Array.length a = 0

let findi_opt f a =
  List.range 0 `To (Array.length a - 1) |> List.find_opt (fun i -> f i a.(i))
