[<AutoOpen>]
module Solver

    let solve (selector: 'a -> bool)
              (filter: 'a -> bool)
              (aggregator: seq<'a> -> 'a)
              (sequence: seq<'a>) =
        sequence
        |> Seq.takeWhile selector
        |> Seq.filter filter
        |> aggregator

    let PassAll _ = true

