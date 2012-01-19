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
            omit; //Problem040.problem040 >> wrap;
            omit; //Problem041.problem041 >> wrap;
            omit; //Problem042.problem042 >> wrap;
            omit; //Problem043.problem043 >> wrap;
            omit; //Problem044.problem044 >> wrap;
            omit; //Problem045.problem045 >> wrap;
            omit; //Problem046.problem046 >> wrap;
            omit; //Problem047.problem047 >> wrap;
            omit; //Problem048.problem048 >> wrap;
            omit; //Problem049.problem049 >> wrap;
            omit; //Problem050.problem050 >> wrap;
            omit; //Problem051.problem051 >> wrap;
            omit; //Problem052.problem052 >> wrap;
            omit; //Problem053.problem053 >> wrap;
            omit; //Problem054.problem054 >> wrap;
            omit; //Problem055.problem055 >> wrap;
            omit; //Problem056.problem056 >> wrap;
            omit; //Problem057.problem057 >> wrap;
            omit; //Problem058.problem058 >> wrap;
            omit; //Problem059.problem059 >> wrap;
            omit; //Problem060.problem060 >> wrap;
            omit; //Problem061.problem061 >> wrap;
            omit; //Problem062.problem062 >> wrap;
            omit; //Problem063.problem063 >> wrap;
            omit; //Problem064.problem064 >> wrap;
            omit; //Problem065.problem065 >> wrap;
            omit; //Problem066.problem066 >> wrap;
            omit; //Problems018_067.problem067 >> wrap;
            omit; //Problem068.problem068 >> wrap;
            omit; //Problem069.problem069 >> wrap;
            omit; //Problem070.problem070 >> wrap;
            omit; //Problem071.problem071 >> wrap;
            Problem072.problem072 >> wrap;
            stub; //Problem073.problem073 >> wrap;
            stub; //Problem074.problem074 >> wrap;
            stub; //Problem075.problem075 >> wrap;
        |]

    [<EntryPoint>]
    let main (args: string[]) =
        for i = 0 to problems.Length - 1 do
            match problems.[i]() with
            | None -> printfn "Project Euler Problem %d solution is not implemented yet" (i + 1)
            | Some(null) -> printfn "Project Euler Problem %d solution is omitted from run" (i + 1)
            | Some(_) as result -> printfn "Project Euler Problem %d Answer: %A" (i + 1) result.Value
        0