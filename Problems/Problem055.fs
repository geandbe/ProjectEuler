// Project Euler Problem 55
// http://projecteuler.net/problem=55
module ProjectEuler.Problem055

open System

let isPalindromic (n: bigint) =
    let isPalindrom (s: string) =
        String(s.ToCharArray() |> Array.rev) = s
    isPalindrom (string n)

let revert (n: bigint) =
    bigint.Parse(String((string n).ToCharArray() |> Array.rev))

let isLychrel (n: bigint) =
    Seq.unfold (fun (i,x) ->
        if isPalindromic x || i > 49 then None
        else Some((i,x), (i+1, x + (revert x))))
        (1, n + (revert n))
    |> Seq.length |> fun x -> if x >= 49 then 1 else 0

let problem055 () =
    [1I..9999I]
    |> Seq.map isLychrel
    |> Seq.sum