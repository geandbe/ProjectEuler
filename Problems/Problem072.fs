// Project Euler Problem 72
// http://projecteuler.net/problem=72
module ProjectEuler.Problem072

let problem072() =
    let d = 1000000
    let ɸ = Array.init (d + 1) id
    for i in 2..d do
        if ɸ.[i] = i then
            for j in i..i..d do
                ɸ.[j] <- ɸ.[j] / i * (i - 1)
    Array.map int64 ɸ |> Array.sum |> (+) -1L