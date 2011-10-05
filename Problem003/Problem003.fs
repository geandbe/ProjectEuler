module ProjectEuler

// Project Euler Problem 3
// http://projecteuler.net/problem=3

//let maxPrimeFactor n =
//    let rec factorize n f =
//        if n <= f then
//            f
//        else
//            match n%f with
//            | 0L -> factorize (n/f) f
//            | _ -> factorize n (f + 1L)
//
//    factorize n 2L
//maxPrimeFactor 600851475143L |> printfn "Problem 3 Answer: %d"

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
        primeFactors 600851475143L |> solve PassAll PassAll Seq.max |> box |> Some