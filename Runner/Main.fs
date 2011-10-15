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
            Problem011.problem011 >> wrap;
            stub;
            stub;
            stub;
        |]


    [<EntryPoint>]
    let main (args: string[]) =
        for i = 0 to problems.Length - 1 do
            match problems.[i]() with
            | None -> printfn "Project Euler Problem %d solution is not implemented yet" (i + 1)
            | Some(null) -> printfn "Project Euler Problem %d solution is omitted from run" (i + 1)
            | Some(_) as result -> printfn "Project Euler Problem %d Answer: %A" (i + 1) result.Value
        0