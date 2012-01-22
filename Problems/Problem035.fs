// Project Euler Problem 35
// http://projecteuler.net/problem=35
module ProjectEuler.Problem035

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

let rotate lst =
    List.tail lst @ [List.head lst]
    
let getRotations lst = 
    let rec getAll lst i =
        if i = 0 then [] else lst :: (getAll (rotate lst) (i - 1))
    getAll lst (List.length lst) 

let disassemble n = n.ToString().ToCharArray() |> Array.toList

let assemble (n: char list) =
    System.Convert.ToInt32(new string(List.toArray n))

//let problem035 () =
//    primes
//    |> solve ((>) 1000000) (disassemble >> getRotations >> Seq.map assemble >> Seq.forall isPrime) Seq.length

let problem035 () =
    primes
    |> Seq.takeWhile ((>) 1000000)
    |> Seq.filter (disassemble >> getRotations >> Seq.map assemble >> Seq.forall isPrime)
    |> Seq.length