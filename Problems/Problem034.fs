// Project Euler Problem 34
// http://projecteuler.net/problem=34
module ProjectEuler.Problem034

let digitFact  = function
    | 0 | 1 -> 1
    | 2 -> 2
    | 3 -> 6
    | 4 -> 24
    | 5 -> 120
    | 6 -> 720
    | 7 -> 5040
    | 8 -> 40320
    | 9 -> 362880
    | _ -> failwith "bad argument"


let maxAsSumOfDigitsFact =
    let nineFact = digitFact 9
    let rec maxN (n: int) =
        if  pown 10 n > nineFact * n then
            nineFact * n
        else
            maxN (n + 1)
    maxN 2

let asSumOfDigitsFact (n: int) =
    Seq.unfold (fun x ->
        if x = 0 then None else Some(digitFact (x%10), x/10) ) n
    |> Seq.sum
    
let problem034 () =
    [10..maxAsSumOfDigitsFact]
    |> List.filter (fun x -> x = asSumOfDigitsFact x)
    |> List.sum