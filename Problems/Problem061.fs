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
    Seq.initInfinite (fun n -> (n, ((Figurate.Generator figurate) >> string) n))
    |> Seq.skip 1
    |> Seq.skipWhile (snd >> String.is4CharLong >> not)
    |> Seq.takeWhile (snd >> String.is4CharLong)
    |> Seq.filter (snd >> String.hasNoZeros)
    |> Seq.map (fun x -> ((snd x).Substring(0,2)), ((snd x).Substring(2)), figurate, (fst x))
    |> Seq.toList

type Member = { Head:string; Tail:string; Poly:Figurate; Order:int }

let problem061 () =
    let prune xs =
        let linkable = Set.intersect (xs |> List.map (fun (h,_,_,_) -> h) |> set) (xs |> List.map (fun (_,t,_,_) -> t) |> set)
        List.filter (fun (h,t,_,_) -> linkable.Contains h && linkable.Contains t) xs

    let members =
        [Triangle;Square;Pentagonal;Hexagonal;Heptagonal;Octagonal]
        |> List.collect generate
        |> prune
        |> List.map (fun (h,t,f,n) -> {Head=h;Tail=t;Poly=f;Order=n})

    let octagonal,others = List.partition (fun x -> x.Poly = Octagonal) members

    [ for i in octagonal do
        for j in others |> List.filter (fun x -> x.Head = i.Tail) do
            for k in others |> List.filter (fun x -> x.Head = j.Tail) do
                for l in others |> List.filter (fun x -> x.Head = k.Tail) do
                    for m in others |> List.filter (fun x -> x.Head = l.Tail) do
                        for n in others |> List.filter (fun x -> x.Head = m.Tail && x.Tail = i.Head) do
                            if [i.Poly;j.Poly;k.Poly;l.Poly;m.Poly;n.Poly] |> set |> Set.count = 6
                               && [i.Order;j.Order;k.Order;l.Order;m.Order;n.Order] |> set |> Set.count = 6 then
                                yield Int32.Parse (i.Head + i.Tail)
                                yield Int32.Parse (j.Head + j.Tail)
                                yield Int32.Parse (k.Head + k.Tail)
                                yield Int32.Parse (l.Head + l.Tail)
                                yield Int32.Parse (m.Head + m.Tail)
                                yield Int32.Parse (n.Head + n.Tail)
    ] |> List.sum
                                    
