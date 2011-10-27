// Project Euler Problem 21
// http://projecteuler.net/problem=21
module ProjectEuler.Problem021

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
    
let isAmicable a = 
    let pair = divisorSum a
    a <> pair && a = divisorSum pair

let problem021 () =
    {1..9999}
    |> solve PassAll isAmicable Seq.sum