// Project Euler Problem 26
// http://projecteuler.net/problem=26
module ProjectEuler.Problem026

let isFullReptent n =
    if n % 2 = 0 || n % 5 = 0 then
        false // n is not relatively prime to 10
    else
        (Seq.initInfinite (fun x -> x+1)
        |> Seq.filter (fun x -> ((pown 10I x) - 1I) % (bigint (int n)) = 0I)
        |> Seq.head) + 1 = n // cycle length is n - 1

let problem026 () =
    [1..999]
    |> solve PassAll isFullReptent Seq.max 