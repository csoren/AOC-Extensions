open Batteries
include Int

let is_positive n = n > 0

let of_bool b = if b then 1 else 0

let clamp n min max =
  if n < min then min
  else if n > max then max
  else n

let sign x =
  if x = 0 then 0
  else if x > 0 then 1
  else -1