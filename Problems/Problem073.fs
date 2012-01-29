// Project Euler Problem 73
// http://projecteuler.net/problem=73

module ProjectEuler.Problem073

let ``preceding1/3`` =
    [1..12000]
    |> List.map (fun x -> (x - 1) / 3, x)
    |> List.maxBy (fun (x,y) -> (float x) / (float y))
   
let problem073() =
    Seq.unfold (fun ((a,b),c,d) ->
        if (c,d) = (1,2) then None
        else
            let temp = int((float (12000 + b)) / (float d)) in
                Some(1, ((c,d),temp*c - a, temp*d - b)))
            (``preceding1/3``, 1, 3)
    |> Seq.sum |> (+) -1



