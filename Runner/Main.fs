namespace ProjectEuler
module Runner =

    let inline wrap any = box any |> Some

    let stub () = None
    let omit () = Some(null)

    let problems : (unit -> obj option) array =
        [|
            omit; //Problem001.problem001 >> wrap;
            omit; //Problem002.problem002 >> wrap;
            omit; //Problem003.problem003 >> wrap;
            omit; //Problem004.problem004 >> wrap;
            omit; //Problem005.problem005 >> wrap;
            omit; //Problem006.problem006 >> wrap;
            omit; //Problem007.problem007 >> wrap;
            omit; //Problem008.problem008 >> wrap;
            omit; //Problem009.problem009 >> wrap;
            omit; //Problem010.problem010 >> wrap;
            omit; //Problem011.problem011 >> wrap;
            omit; //Problem012.problem012 >> wrap;
            omit; //Problem013.problem013 >> wrap;
            omit; //Problem014.problem014 >> wrap;
            omit; //Problem015.problem015 >> wrap;
            omit; //Problem016.problem016 >> wrap;
            omit; //Problem017.problem017 >> wrap;
            omit; //Problems018_067.problem018 >> wrap;
            Problem019.problem019 >> wrap;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            stub;
            Problems018_067.problem067 >> wrap;
        |]

    [<EntryPoint>]
    let main (args: string[]) =
        for i = 0 to problems.Length - 1 do
            match problems.[i]() with
            | None -> printfn "Project Euler Problem %d solution is not implemented yet" (i + 1)
            | Some(null) -> printfn "Project Euler Problem %d solution is omitted from run" (i + 1)
            | Some(_) as result -> printfn "Project Euler Problem %d Answer: %A" (i + 1) result.Value
        0