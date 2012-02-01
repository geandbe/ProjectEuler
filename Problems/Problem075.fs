// Project Euler Problem 75
// http://projecteuler.net/problem=75

module ProjectEuler.Problem075

// Euclidean algorithm for gcd(n,m)
let rec gcd n m =
    match m with
    | 0 -> n
    | _ -> gcd m (n % m)

let maxPerimeter = 1500000
let maxSide = maxPerimeter/2 |> (float >> sqrt >> int)

let problem075() =
    let allPerimeters: int[] = Array.zeroCreate (maxPerimeter + 1)
    let rec multiples p' p =
        if p' <= maxPerimeter then
            if allPerimeters.[p'] = 0 then
                allPerimeters.[0] <- allPerimeters.[0] + 1
                allPerimeters.[p'] <- 1
            elif allPerimeters.[p'] = 1 then
                allPerimeters.[0] <- allPerimeters.[0] - 1
                allPerimeters.[p'] <- -1
            multiples (p' + p) p
        
//  Euclid's formula for generating Pythagorean triples
//  (http://en.wikipedia.org/wiki/Pythagorean_triple) 
    for m in 2..maxSide do
        for n in 1..(m - 1) do
            if (n + m) % 2 = 1 && (gcd n m = 1) then
                multiples (2 * (m*m + n*m)) (2 * (m*m + n*m))
    
    allPerimeters.[0]
    
