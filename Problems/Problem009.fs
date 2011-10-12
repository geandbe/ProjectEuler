// Project Euler Problem 9
// http://projecteuler.net/problem=9
namespace ProjectEuler
module Problem009 =

    let tripletsSum1000 =
        seq {
        for a in 1 .. 333 do
            for b in a + 1 .. 499 do
                let c = 1000 - b - a
                if a < b && b < c then
                    yield (a,b,c)
        }

    let problem009 () =
        tripletsSum1000
        |> solve PassAll (fun (a,b,c) -> a * a + b * b = c * c) (Seq.head >> fun (a,b,c) -> a*b*c)