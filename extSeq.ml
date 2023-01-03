open Batteries

include Seq

let ncycle n (s: 'a Seq.t): 'a Seq.t =
  Seq.init n (fun _ -> s) |> Seq.flatten
