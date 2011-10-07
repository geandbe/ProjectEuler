// Project Euler Problem 5
// http://projecteuler.net/problem=5
module ProjectEuler

// gcd - greatest common divisor; Euclidean algorithm:
let rec gcd n m =
    match m with
    | 0L -> n
    | _ -> gcd m (n % m)

// lcm - least common multiple
let lcm n m = n * m / gcd n m

let problem005 () =
    seq [1L .. 20L]
    |> solve PassAll PassAll (Seq.reduce lcm)