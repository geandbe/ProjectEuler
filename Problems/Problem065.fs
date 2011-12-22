// Project Euler Problem 65
// http://projecteuler.net/problem=65
module ProjectEuler.Problem065

let problem065 () =
    Seq.initInfinite (fun x -> seq [1I; 2I*(bigint x); 1I])
    |> (Seq.skip 1 >> Seq.collect id >> Seq.skip 1)
    |> Seq.scan (fun (``n(i)``, ``n(i-1)``) i ->
                                    (i * ``n(i)`` + ``n(i-1)``, ``n(i)``))
                                    (3I, 2I)
    |> (Seq.skip 98 >> Seq.head >> fst >> string >>
            Seq.map (string >> int) >> Seq.sum)