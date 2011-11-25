// Project Euler Problem 36
// http://projecteuler.net/problem=36
module ProjectEuler.Problem036

open System

let isPalindrom (s: string) =
    String(s.ToCharArray() |> Array.rev) = s

let isPalindromic (n: int) =
    isPalindrom (Convert.ToString (n, 2)) && isPalindrom (string n)

let problem036 () =
    {1..999999}
    |> solve PassAll isPalindromic Seq.sum
