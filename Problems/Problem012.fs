// Project Euler Problem 12
// http://projecteuler.net/problem=12
module ProjectEuler.Problem012

// Build a sequence of triangle numbers within the integer field
let triangles =
    Seq.unfold (fun (x, acc) -> Some (acc + x, (x + 1, acc + x))) (1,0)

// Find prime factors of a number within the integer field as a sequence of tuples (primeFactor, exponent)
let primeFactors number =
    let rec factorize number factor answer =
        if number <= factor then
            factor :: answer
        elif number % factor = 0 then
            factorize (number/factor) factor (factor :: answer)
        else
            factorize number (factor+1) answer
    factorize number 2 [] |> Seq.countBy id

// Find number of all divisors of an integer
// After prime factorization the sought value equals to multiple of prime exponents, each incremented by 1,
// reference: http://en.wikipedia.org/wiki/Divisor_function
let numberOfDivisors number =
    primeFactors number
    |> Seq.fold (fun acc (factor, exp) -> acc * (exp + 1)) 1

// Finally, solve the Problem 12
let problem012 () =
    triangles
    |> solve PassAll PassAll (Seq.tryFind (fun t -> (numberOfDivisors t) > 500) >> Option.get)
