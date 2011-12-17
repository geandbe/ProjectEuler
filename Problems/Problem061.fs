// Project Euler Problem 61
// http://projecteuler.net/problem=61
module ProjectEuler.Problem061

open System

type String with
    static member is4CharLong x = (String.length x) = 4
    static member hasNoZeros (x: String) = x.IndexOf('0') = -1

type Figurate =
    | Triangle
    | Square
    | Pentagonal
    | Hexagonal
    | Heptagonal
    | Octagonal
    static member Generator = function
        | Triangle -> fun n -> n * (n + 1) / 2
        | Square -> fun n -> n * n
        | Pentagonal -> fun n -> n * (3 * n - 1) / 2
        | Hexagonal -> fun n -> n * (2 * n - 1)
        | Heptagonal -> fun n -> n * (5 * n - 3) / 2
        | Octagonal -> fun n -> n * (3 * n - 2)

let generate figurate =
    Seq.initInfinite ((Figurate.Generator figurate) >> string)
    |> Seq.skipWhile (String.is4CharLong >> not)
    |> Seq.takeWhile String.is4CharLong
    |> Seq.filter String.hasNoZeros
    |> Seq.map (fun x -> (x.Substring(0,2), x.Substring(2), figurate))
    |> Seq.toList

type Link = { Head:string; Tail:string; Poly:Figurate }

let problem061 () =
    let prune xs =
        let linkable = Set.intersect (xs |> List.map (fun (h,_,_) -> h) |> set)
                                     (xs |> List.map (fun (_,t,_) -> t) |> set)
        List.filter (fun (h,t,_) -> linkable.Contains h && linkable.Contains t) xs

    let links =
        [Triangle;Square;Pentagonal;Hexagonal;Heptagonal;Octagonal]
        |> List.collect generate
        |> prune
        |> List.map (fun (h,t,f) -> {Head=h;Tail=t;Poly=f})

    let octagonal,others = List.partition (fun x -> x.Poly = Octagonal) links

    [ for i in octagonal do
        for j in others |> List.filter (fun x -> x.Head = i.Tail) do
            for k in others |> List.filter (fun x -> x.Head = j.Tail) do
                for l in others |> List.filter (fun x -> x.Head = k.Tail) do
                    for m in others |> List.filter (fun x -> x.Head = l.Tail) do
                        for n in others |> List.filter (fun x -> x.Head = m.Tail && x.Tail = i.Head) do
                            if set [i.Poly;j.Poly;k.Poly;l.Poly;m.Poly;n.Poly] |> Set.count = 6 then
                                yield i.Head + i.Tail
                                yield j.Head + j.Tail
                                yield k.Head + k.Tail
                                yield l.Head + l.Tail
                                yield m.Head + m.Tail
                                yield n.Head + n.Tail
    ] |> List.map Int32.Parse |> List.sum
                                    
