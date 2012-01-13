// Project Euler Problem 71
// http://projecteuler.net/problem=71
module ProjectEuler.Problem071

let problem071() =
    [1..1000000]
    |> List.map (fun x -> ((3 * x - 1) / 7), x)
    |> List.maxBy (fun (x,y) -> (float x) / (float y))
    |> fst