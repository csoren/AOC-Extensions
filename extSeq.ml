open Batteries

include Seq

let ncycle n (s: 'a Seq.t): 'a Seq.t =
  Seq.init n (fun _ -> s) |> Seq.flatten

let min_max s =
  let mm (min, max) el =
    let min = if Stdlib.compare min el < 0 then min else el in
    let max = if Stdlib.compare max el > 0 then max else el in
    (min, max)
  in
  let h = Seq.hd s in
  Seq.fold_left mm (h, h) (Seq.tl s)
