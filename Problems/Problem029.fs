// Project Euler Problem 29
// http://projecteuler.net/problem=29
module ProjectEuler.Problem029

open System

let problem029 () =
    [for i = 2. to 100. do for j = 2. to 100. do yield Math.Pow (i,j)]
    |> set |> Set.count