// Project Euler Problem 6
// http://projecteuler.net/problem=6

module ProjectEuler.Problem006

let problem006 () =
    [0..100]
    |> List.fold (fun (sum, sumsqrs) s -> (sum + s, sumsqrs + s * s))(0, 0)
    |> fun (sum,sumsqrs) -> sum * sum - sumsqrs