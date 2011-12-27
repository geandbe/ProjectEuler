// Project Euler Problem 68
// http://projecteuler.net/problem=68
module ProjectEuler.Problem068

let isMagic5gon os is =
    List.zip3 os is (List.tail is @ [List.head is])
    |> List.forall (fun (o,i,i') -> o + i + i' = 14)

let glueSolution os is =
    List.zip3 os is (List.tail is @ [List.head is])
    |> List.fold (fun acc (o,i,i') -> sprintf "%s%d%d%d" acc o i i') ""

//---J.Harrop F# for Scientists; pp.166-167---------
let rec distribute e = function
   | [] -> [[e]]
   | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
and permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
//--------------------------------------------------

let problem068 () =
    let candidates = permute [1;2;3;4;5] |> List.filter (isMagic5gon [6;10;9;8;7])
    if List.length candidates= 1 then
        candidates |> List.head |> glueSolution [6;10;9;8;7]
    else
        failwith "Bad solution approach"