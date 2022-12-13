open Batteries

include Option

let or_else f = function
  | None -> f ()
  | o -> o

let tee f v =
  match v with
    | Some o -> f o; v
    | None -> None
