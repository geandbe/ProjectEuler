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
            omit; //Problem023.problem023 >> wrap;
            omit; //Problem024.problem024 >> wrap;
            omit; //Problem025.problem025 >> wrap;
            omit; //Problem026.problem026 >> wrap;
            omit; //Problem027.problem027 >> wrap;
            omit; //Problem028.problem028 >> wrap;
            omit; //Problem029.problem029 >> wrap;
            omit; //Problem030.problem030 >> wrap;
            omit; //Problem031.problem031 >> wrap;
            omit; //Problem032.problem032 >> wrap;
            omit; //Problem033.problem033 >> wrap;
            omit; //Problem034.problem034 >> wrap;
            omit; //Problem035.problem035 >> wrap;
            omit; //Problem036.problem036 >> wrap;
            omit; //Problem037.problem037 >> wrap;
            omit; //Problem038.problem038 >> wrap;
            omit; //Problem039.problem039 >> wrap;
            Problem040.problem040 >> wrap;
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