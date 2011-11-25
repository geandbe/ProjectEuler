// Project Euler Problem 45
// http://projecteuler.net/problem=45
module ProjectEuler.Problem045

// Positive root of x*(3*x - 1)/2 = y is x = (1 + sqrt(1 + 24*y))/6
// Positive root of x*(2*x - 1) = y is x = (1 + sqrt(1 + 8*y))/4

open System

let sqrtn n = n |> float |> sqrt |> int64

let makePentagonal n =
    n*(3L*n - 1L)/2L

let isPentagonal x =
    x = makePentagonal ((1L + sqrtn(24L*x + 1L))/6L)

let makeHexagonal n =
    n*(2L*n - 1L)

let isHexagonal x =
    x = makeHexagonal ((1L + sqrtn(8L*x + 1L))/4L)

let makeTriangle n =
    n * (n + 1L) / 2L

let problem045 () =
    seq { 286L .. Int64.MaxValue }
    |> Seq.skipWhile (fun x -> (not (isPentagonal(makeTriangle(x))))
                            || (not (isHexagonal(makeTriangle(x)))))
    |> Seq.head
    |> fun x -> makeTriangle x
