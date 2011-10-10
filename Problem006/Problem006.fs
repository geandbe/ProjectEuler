// Project Euler Problem 6
// http://projecteuler.net/problem=6
module ProjectEuler

let aggregate ss =
    ss
    |> Seq.fold (fun (sum, sumsqrs) s -> (sum + s, sumsqrs + s * s))(0, 0)
    |> fun (sum,sumsqrs) -> sum * sum - sumsqrs

let problem006 () =
    Seq.initInfinite id
    |> solve ((>=) 100) PassAll aggregate
