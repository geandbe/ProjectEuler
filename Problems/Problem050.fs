// Project Euler Problem 50
// http://projecteuler.net/problem=50
module ProjectEuler.Problem050

#nowarn "40"

let rec primes = 
    Seq.cache <| seq { yield 2; yield! Seq.unfold nextPrime 3 }
and nextPrime n =
    if isPrime n then Some(n, n + 2) else nextPrime(n + 2)
and isPrime n =
    if n >= 2 then
        primes 
        |> Seq.tryFind (fun x -> n % x = 0 || x * x > n)
        |> fun x -> x.Value * x.Value > n
    else false

let problem050 () =
    let primeArray = primes |> Seq.takeWhile ((>) 1000000) |> Seq.toArray 
    let primeCount = primeArray.Length
    let mutable sum = 0
    let mutable length = 0
    let mutable maxLength = 0
    let mutable result = 0

    for i = 0 to primeCount - 1 do
        sum <- primeArray.[i]
        length <- 1
        let mutable j = i + 1
        while j <= primeCount - 1 && sum < 1000000 do
            sum <- sum + primeArray.[j]
            length <- length + 1
            if isPrime sum then
                if length > maxLength then
                    result <- sum
                    maxLength <- length
            j <- j + 1

    result