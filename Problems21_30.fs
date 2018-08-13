module Problems21_30

    open System

    let problem21() = 
        let divisorsSum n =  Numeric.divisors n |> Seq.sum
        seq { 
                for i in 1..9999 do
                    let b =  divisorsSum i
                    let c = divisorsSum b
                    if i <> b && i = c then 
                        yield! [i; b]
        } |> Seq.distinct |> Seq.sum

    let problem22 (data:string) = 
        let weight (str:string) = str |> Seq.fold (fun s c -> s + (int c) - (int 'A') + 1) 0
        data.Split(',') |> Seq.sort |> Seq.mapi (fun i s -> (i+1)*weight s ) |> Seq.sum

    let problem23() = 
        let isAbundant x = Numeric.divisors x |> Seq.sum > x
        let lst = [for i in 1..28123 do if isAbundant i then yield i]
        let sums = new Collections.Generic.HashSet<int>([for i in lst do for j in lst do yield i+j])
        [for i in 1..28123 do if not(sums.Contains i) then yield i] |> Seq.sum

    let problem24() = Numeric.perms [0..9] |> Seq.skip 999999 |> Seq.head |> Seq.toList

    let problem25() = 
        let digitsCount n = 
            let mutable i = n
            let mutable count = 0 
            while i > 0I do 
                i <- i/10I
                count <- count + 1
            count
        Numeric.fib |> Seq.takeWhile (fun f -> digitsCount f < 1000) |> Seq.length

    let problem26() =
        let run n =
            let mutable temp = 1
            let mutable brk = false
            let numbers = new FSharp.Collections.ResizeArray<int>()
            let processed = new Collections.Generic.HashSet<int>()
            while not brk do 
                let mutable addZero = false
                while temp < n do 
                    temp <- temp*10
                    if addZero then 
                        numbers.Add 0
                        addZero <-false
                    else 
                        addZero <- true
                if processed.Contains(temp) then brk <- true
                else 
                    let _ = processed.Add(temp)
                    let rem = ref 0
                    temp <- Math.DivRem(temp, n, rem)
                    numbers.Add temp
                    if !rem = 0 || temp = n then brk <- true
                    else temp <- !rem
            (n, numbers)
        [2..999] |> Seq.map (fun i -> run i) |> Seq.maxBy (fun (_, l) -> Seq.length l) |> fst

    let problem27() = 
        let f n a b = n*n + a*n + b
        let maxCoeff a b = Seq.initInfinite (fun n -> f n a b) |> Seq.takeWhile Primes.fastPrimeCheckInt |> Seq.length
        seq {
            for a in -999..999 do
                    for b in -1000..1000 do
                        yield (a, b), maxCoeff a b
        } |> Seq.maxBy snd |> fst |> fun (a,b) -> a*b

    let problem100() = 
        0
