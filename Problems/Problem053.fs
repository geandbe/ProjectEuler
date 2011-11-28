// Project Euler Problem 53
// http://projecteuler.net/problem=53
module ProjectEuler.Problem053

let fact (n: bigint) =
    [1I..n] |> List.fold (*) 1I

let combinations r n =
    (fact n)/((fact r)*(fact (n-r)))

let problem053 () =
    seq {
        for n in 1I..100I do
            yield ( [1I..n]
            |> Seq.tryFind (fun x -> combinations x n > 1000000I)
            |> function | None -> 0I | _ as x -> n + 1I - 2I*x.Value)
        }
    
    |> Seq.sum