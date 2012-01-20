// Project Euler Problem 15
// http://projecteuler.net/problem=15
module ProjectEuler.Problem015

let fact n = [1I..n] |> Seq.reduce (*)

//let problem015 () =
//    Seq.empty
//    |> solve PassAll PassAll (fun _ -> fact 40I / ((fact 20I) * (fact 20I)))

let problem015 () =
    fact 40I / ((fact 20I) * (fact 20I))