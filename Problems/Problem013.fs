// Project Euler Problem 13
// http://projecteuler.net/problem=13
module ProjectEuler.Problem013

open System.IO

let readLines (filePath: string) =
    seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield bigint.Parse(sr.ReadLine ())
        } 

//let problem013 () =
//    readLines @"..\..\..\Datafiles\problem013.data"
//    |> solve PassAll PassAll (Seq.sum >> sprintf "%A" >> fun x -> x.Substring(0,10))

let problem013 () =
    readLines @"..\..\..\Datafiles\problem013.data"
    |> (Seq.sum >> sprintf "%A" >> fun x -> x.Substring(0,10))