// Project Euler Problem 64
// http://projecteuler.net/problem=64
module ProjectEuler.Problem064

// Recurrrent equation to calculate period of periodic square root:
// http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion

let oddPeriod s =
    let limit = int (sqrt (float s))
    if limit * limit  = s then
        false
    else
        Seq.initInfinite id
        |> Seq.scan (fun (d, m, _) i ->
                        let d = (s - m * m) / d in
                            (d, ((limit + m) / d) * d - m, i + 1)) (1, limit, 0)
        |> Seq.skip 1 |> Seq.skipWhile (fun (d, _, _) -> d <> 1) |> Seq.head
        |> fun (_, _, p) -> p % 2 = 1

let problem064 () =
    [2..10000]
    |> Seq.filter oddPeriod
    |> Seq.map (fun x -> 1)
    |> Seq.sum