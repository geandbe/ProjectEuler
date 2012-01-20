// Project Euler Problem 2
// http://projecteuler.net/problem=2

module ProjectEuler.Problem002

let inline multipleOf divider number = number % divider = 0
let multipleOf2 = multipleOf 2

let fibnums = Seq.unfold (fun (current, next) ->
                    Some(current+next, (next, current+next)))(1,1)
                |> Seq.append (seq[0;1;1])

// solution based on solver
//let problem002 () =
//    fibnums
//    |> solve ((>=) 4000000) multipleOf2 Seq.sum

let problem002 () =
    fibnums
    |> Seq.takeWhile ((>=) 4000000)
    |> Seq.filter multipleOf2
    |> Seq.sum