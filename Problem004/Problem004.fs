module ProjectEuler

// Project Euler Problem 4
// http://projecteuler.net/problem=4

open System

let allProducts l1 l2 =
    List.map (fun x -> (List.map (fun y -> x * y) l2)) l1 |> List.concat

let isAnagram (s: string) =
    new String(s.ToCharArray() |> Array.rev) = s

let isPalindromic n = 
    isAnagram(string n)

let problem004 () =
    allProducts [100..999] [100..999] |> Seq.distinct
    |> solve PassAll isPalindromic Seq.max

