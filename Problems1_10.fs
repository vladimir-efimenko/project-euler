module Problems1_10

    open Primes
    open Problems

    let problem1() = [for i in 1..999 do if i%3 = 0 || i%5 = 0 then yield i] |> Seq.sum

    let problem2() = 
        seq {
            let a = ref 1
            let b = ref 1
            while true do
                let temp = !a
                yield temp
                a := !b + !a
                b := temp
        }
        |> Seq.takeWhile (fun x-> x <= 4000000)
        |> Seq.where (fun x -> x%2 = 0)
        |> Seq.sum

    let problem3() = 600851475143I |> factorize |> Seq.last |> fst

    let problem4() = 
        let allNumbers = 
            seq {
                for i = 999 downto 100 do
                for j = 999 downto 100 do
                yield i*j
            }
        let isPalindrome num =
            let str = num.ToString()
            let mutable res = true
            for i = 0 to str.Length/2 do 
                if str.[i] <> str.[str.Length - 1- i] then res <- false
            res
        allNumbers |> Seq.where isPalindrome |> Seq.max

    let problem5() = [1I..20I] |> Seq.fold (fun s x -> Numeric.lcm s x) 1I

    let problem6() = 
        let sqSum = [1..100] |> Seq.map (fun x -> x*x) |> Seq.sum
        let sum = [1..100] |> Seq.sum 
        sum*sum - sqSum

    let problem7() = primesGen |> Seq.skip 10000 |> Seq.head

    let problem8 (d:string) adjLength = 
        let mutable i = 0
        let mutable maxProd = 0I
        while i+adjLength < d.Length do 
            let prod = d.Substring(i, adjLength) |> Seq.fold (fun s c -> s*c.toBigInt) 1I
            maxProd <- if prod > maxProd then prod else maxProd
            i <- i + 1
        maxProd

    let problem9 x = 
        seq {
            for a in 1..x do 
            for b in a+1..x do 
            let c = a*a + b*b |> float |> System.Math.Sqrt
            if c = floor c then 
                if a + b + int(c) = x then yield a*b*int(c)
        }

    let problem10() = primesGen |> Seq.takeWhile (fun x-> x < 2000000I) |> Seq.sum