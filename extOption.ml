open Batteries

include Option

let or_else f = function
  | None -> f ()
  | o -> o
