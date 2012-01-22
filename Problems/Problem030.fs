// Project Euler Problem 30
// http://projecteuler.net/problem=30
module ProjectEuler.Problem030

let maxAsSumOfDigitsPow5 =
    let ninePow5 = 9*9*9*9*9
    let rec maxN (n: int) =
        if  pown 10 n > ninePow5 * n then
            ninePow5 * n
        else
            maxN (n + 1)
    maxN 2

let asSumOfDigitsPow5 (n: int) =
    Seq.unfold (fun x -> if x = 0 then None else Some(pown (x%10) 5, x/10) ) n
    |> Seq.sum

//let problem030 () =
//    {10..maxAsSumOfDigitsPow5}
//    |> solve PassAll (fun x -> x = asSumOfDigitsPow5 x) Seq.sum

let problem030 () =
    {10..maxAsSumOfDigitsPow5}
    |> Seq.filter (fun x -> x = asSumOfDigitsPow5 x) |> Seq.sum