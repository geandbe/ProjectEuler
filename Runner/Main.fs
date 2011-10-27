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
            omit; //Problem019.problem019 >> wrap;
            omit; //Problem020.problem020 >> wrap;
            omit; //Problem021.problem021 >> wrap;
            omit; //Problem022.problem022 >> wrap;
            Problem023.problem023 >> wrap;
            Problem024.problem024 >> wrap;
            stub; // Problem 25
            stub; // Problem 26
            stub; // Problem 27
            stub; // Problem 28
            stub; // Problem 29
            stub; // Problem 30
            stub; // Problem 31
            stub; // Problem 32
            stub; // Problem 33
            stub; // Problem 34
            stub; // Problem 35
            stub; // Problem 36
            stub; // Problem 37
            stub; // Problem 38
            stub; // Problem 39
            stub; // Problem 40
            stub; // Problem 41
            stub; // Problem 42
            stub; // Problem 43
            stub; // Problem 44
            stub; // Problem 45
            stub; // Problem 46
            stub; // Problem 47
            stub; // Problem 48
            stub; // Problem 49
            stub; // Problem 50
            stub; // Problem 51
            stub; // Problem 52
            stub; // Problem 53
            stub; // Problem 54
            stub; // Problem 55
            stub; // Problem 56
            stub; // Problem 57
            stub; // Problem 58
            stub; // Problem 59
            stub; // Problem 60
            stub; // Problem 61
            stub; // Problem 62
            stub; // Problem 63
            stub; // Problem 64
            stub; // Problem 65
            stub; // Problem 66
            omit; //Problems018_067.problem067 >> wrap;
        |]

    [<EntryPoint>]
    let main (args: string[]) =
        for i = 0 to problems.Length - 1 do
            match problems.[i]() with
            | None -> printfn "Project Euler Problem %d solution is not implemented yet" (i + 1)
            | Some(null) -> printfn "Project Euler Problem %d solution is omitted from run" (i + 1)
            | Some(_) as result -> printfn "Project Euler Problem %d Answer: %A" (i + 1) result.Value
        0