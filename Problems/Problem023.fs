// Project Euler Problem 23
// http://projecteuler.net/problem=23
module ProjectEuler.Problem023

let divisorSum n =
    let divisors n =
        [
            yield 1
            for i in 2..int (sqrt(float n)) do
                if n%i = 0 then
                    yield i
                    if i <> n/i then yield n/i
        ]
    
    divisors n |> List.sum

let isAbundant n =
    n < divisorSum n

let fastSolution abundant =
    let numbers = Array.create 28123 0
    
    let rec markPresentable l =
        match l with
        | [] -> ()
        | x::xs ->
           List.iter (fun a ->
                     if x + a <= 28123 then numbers.[x + a - 1] <- 1) l
           markPresentable xs
    
    markPresentable abundant
    
    [for i in 0..28122 do if numbers.[i] = 0 then yield i + 1] |> List.sum

//let problem023 () =
//    [1..28123]
//    |> solve PassAll isAbundant (Seq.toList >> fastSolution)

let problem023 () =
    [1..28123]
    |> Seq.filter isAbundant |> (Seq.toList >> fastSolution)