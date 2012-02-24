// Project Euler Problem 78
// http://projecteuler.net/problem=78

module ProjectEuler.Problem078

// http://mathworld.wolfram.com/PartitionFunctionP.html
//    item (11) - Euler's generating function
let problem078 () =
    let ``P(n)`` (cache: int[]) n  =
        let rec ``while/break`` k =
            if k <= n then
                let idx1 = n - k * (3 * k - 1) / 2
                let idx2 = n - k * (3 * k + 1) / 2
                if idx1 >= 0 || idx2 >= 0 then
                    let sign = if k%2 = 0 then -1 else 1
                    if idx1 >= 0 then cache.[n] <- cache.[n] + sign*cache.[idx1]
                    if idx2 >= 0 then cache.[n] <- cache.[n] + sign*cache.[idx2]
                    cache.[n] <- cache.[n] % 1000000
                    ``while/break`` (k + 1)
        ``while/break`` 1
        cache.[n]

    let cache = Array.zeroCreate 100000
    cache.[0] <- 1
    Seq.initInfinite id |> Seq.find (fun n -> ``P(n)`` cache n = 0)

let problem078I () =
    let ``P(n)`` (cache: int[]) n  =
        let mutable recur = true
        let mutable k = 1
        while k <= n && recur do
            let idx1 = n - k * (3 * k - 1) / 2
            let idx2 = n - k * (3 * k + 1) / 2
            if idx1 >= 0 || idx2 >= 0 then
                let sign = if k%2 = 0 then -1 else 1
                if idx1 >= 0 then cache.[n] <- cache.[n] + sign*cache.[idx1]
                if idx2 >= 0 then cache.[n] <- cache.[n] + sign*cache.[idx2]
                cache.[n] <- cache.[n] % 1000000
                k <- k + 1
            else recur <- false
        cache.[n]

    let cache = Array.zeroCreate 100000
    cache.[0] <- 1
    Seq.initInfinite id |> Seq.find (fun n -> ``P(n)`` cache n = 0)
