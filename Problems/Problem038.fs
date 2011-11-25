// Project Euler Problem 38
// http://projecteuler.net/problem=38
module ProjectEuler.Problem038

let is1To9Pandigital (s: string) =
        let ss = s |> Set.ofSeq
        s.Length = 9 && Set.count ss = 9 && ss.MinimumElement = '1' && ss.MaximumElement = '9'
let concatenatedProduct n =
    [for i = 1 to 9 do yield string(n * i)]
    |> List.fold (fun (res:string) x -> if res.Length < 9 then res + x else res) ""

let problem038 () =
    [1..9999]
    |> List.map concatenatedProduct
    |> List.filter is1To9Pandigital
    |> List.max