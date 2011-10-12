// Project Euler Problem 1
// http://projecteuler.net/problem=1
namespace ProjectEuler

module Problem001 =

//--------------------------------------------------------------------//
//printfn "(1) Problem 1 Answer: %d" (
//    List.sum [3..3..999]
//    + List.sum [5..5..999]
//    - List.sum [15..15..999])
////--------------------------------------------------------------------//
//let inline (%%) x y = x % y = 0
//
//[for x in 1..999 do if x %% 3 || x %% 5 then yield x]
//|> List.sum
//|> printfn "(2) Problem 1 Answer: %d"
////--------------------------------------------------------------------//
//[3..3..999] @ [5..5..999]
//|> set
//|> Set.fold (+) 0
//|> printfn "(3) Problem 1 Answer: %d"
////--------------------------------------------------------------------//
//let inline multipleOf y x = x % y = 0
//let multipleOf3, multipleOf5 = multipleOf 3, multipleOf 5
//let multipleOf3Or5 = (fun x -> multipleOf3 x || multipleOf5 x)
//
//Seq.initInfinite id
//|> Seq.takeWhile ((>) 1000)
//|> Seq.filter (multipleOf3Or5)
//|> Seq.sum
//|> printfn "(4) Problem 1 Answer: %d"
//--------------------------------------------------------------------//

// And solution based on usage of Solver
    let problem001 () =
        (Seq.initInfinite id)
        |> solve ((>) 1000) (fun x -> x % 3 = 0 || x % 5 = 0) Seq.sum
