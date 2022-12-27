open Batteries

include Set

let size s =
  Set.to_array s |> Array.length
