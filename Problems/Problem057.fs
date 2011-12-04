// Project Euler Problem 57
// http://projecteuler.net/problem=57
module ProjectEuler.Problem057

let numberOfDigits n = n.ToString().ToCharArray().GetLength(0)
    
let expansions = Seq.unfold (fun (n,d) -> Some((n+2I*d,n+d),(n+2I*d,n+d)))(1I,1I)

let problem057 () =
    expansions
    |> Seq.take 1000
    |> Seq.filter (fun (numerator, denominator) ->
                    numberOfDigits numerator > numberOfDigits denominator)
    |> Seq.length