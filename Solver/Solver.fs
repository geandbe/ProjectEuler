namespace ProjectEuler

[<AutoOpen>]
module Solver =

    let solve (selector: 'a -> bool)
              (filter: 'a -> bool)
              (aggregator: seq<'a> -> 'b)
              (sequence: seq<'a>) =
        sequence
        |> Seq.takeWhile selector
        |> Seq.filter filter
        |> aggregator

    let PassAll _ = true
