// Project Euler Problem 28
// http://projecteuler.net/problem=28
module ProjectEuler.Problem028

let problem028 () =
    [1..500]
    |> List.fold (fun (accum, last) n -> (accum + 4*last + 20*n, last + 8*n)) (1,1)
    |> fst
