// Project Euler Problem 3
// http://projecteuler.net/problem=3
namespace ProjectEuler
module Problem003 =

    let primeFactors n =
        let rec factorize n f =
            seq {
            if n <= f then
                yield f
            else
                match n%f with
                | 0L -> yield f; yield! factorize (n/f) f
                | _ -> yield! factorize n (f + 1L)
            }

        factorize n 2L

    let problem003 () =
        primeFactors 600851475143L
        |> solve PassAll PassAll Seq.max