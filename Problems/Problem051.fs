// Project Euler Problem 51
// http://projecteuler.net/problem=51
module ProjectEuler.Problem051

open System

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

let of3Digits0To2 (s: string) =
    let counts = [| (s.Split('0').Length - 1);
                    (s.Split('1').Length - 1);
                    (s.Split('2').Length - 1); |]
    if counts.[1] = 3 && s.Substring(5) = "1" then false
    else counts.[0] >= 3 || counts.[1] >= 3 || counts.[2] >= 3

let candidates =
    primes
    |> Seq.takeWhile ((>) 1000000)
    |> Seq.filter ((<) 100000)
    |> Seq.map string
    |> Seq.filter of3Digits0To2

let has7Transforms (s: string) =
    let variant =
        if s.Split('0').Length - 1 >= 3 then
           ("0",["1";"2";"3";"4";"5";"6";"7";"8";"9"])
        elif s.Split('1').Length - 1 >= 3 then
           ("1",["0";"2";"3";"4";"5";"6";"7";"8";"9"])
        else
           ("2",["0";"1";"3";"4";"5";"6";"7";"8";"9"])

    Seq.length
        (seq {
            for i in (snd variant) do
                let candidate = Int32.Parse(s.Replace((fst variant), i))
                if isPrime candidate && candidate >= 100000 then
                    yield candidate
              }) >= 7

let problem051 () =
    candidates
    |> Seq.find has7Transforms