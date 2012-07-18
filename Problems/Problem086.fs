// Project Euler Problem 86
// http://projecteuler.net/problem=86
module ProjectEuler.Problem086

let combinations l ``h + w`` =
    if l >= ``h + w`` then ``h + w``/ 2
    else l - (``h + w`` + 1) / 2 + 1

let isIntegerHypotenuse (cathetus1,cathetus2) =
    let hypotenuse = sqrt (float (cathetus1 * cathetus1 + cathetus2 * cathetus2))
    hypotenuse = floor hypotenuse

let problem086() =
    Seq.initInfinite id
    |> Seq.collect (fun l -> seq {for ``h + w`` in [2..2*l] -> (l,``h + w``)})
    |> Seq.filter isIntegerHypotenuse
    |> Seq.scan (fun (paths, M) (l, ``h + w``) -> (paths + (combinations l ``h + w``), l))(0,0)
    |> Seq.skipWhile (fun (paths, M) -> paths < 1000000)
    |> Seq.head
    |> snd