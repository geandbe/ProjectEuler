// Project Euler Problem 76
// http://projecteuler.net/problem=76

module ProjectEuler.Problem076

let problem076() =
    let n = 100
    let partitions = Array.zeroCreate (n + 1)
    partitions.[0] <- 1
    for addendum in 1..n - 1 do
        for i in addendum..n do
            partitions.[i] <- partitions.[i] + partitions.[i - addendum]

    partitions.[n] 

