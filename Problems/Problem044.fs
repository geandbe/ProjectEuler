// Project Euler Problem 44
// http://projecteuler.net/problem=44
module ProjectEuler.Problem044

// Positive root of x*(3*x - 1)/2 = y is x = (1 + sqrt(1 + 24*y))/6

open System

let upperBound = 20000

let sqrtn n = n |> float |> sqrt |> int

let makePentagonal n =
    n*(3*n - 1)/2

let isPentagonal x =
    x = makePentagonal ((1 + sqrtn(24*x + 1))/6)

let pentagonals: int array = Array.zeroCreate upperBound

let mutable result = Int32.MaxValue
let mutable delta = 0

let problem044 () =
    for i = 0 to upperBound - 1 do
        pentagonals.[i] <- makePentagonal (i+1)
        for j = 0 to i - 1 do
            if isPentagonal(pentagonals.[i] + pentagonals.[j]) && isPentagonal(pentagonals.[i] - pentagonals.[j]) then
                delta <- pentagonals.[i] - pentagonals.[j]
                if result > delta then
                    result <- delta
    result

