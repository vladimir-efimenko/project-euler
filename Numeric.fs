module Numeric

    let rec gcd a b = 
            if b = 0I then a 
            else gcd b (a%b)

    let lcm a b = (a*b)/(gcd a b)

    let isOdd n = n%2I <> 0I

    let isEven n = not(isOdd n)

    let binom n k = 
        let mutable res = 1I
        for i in 1I..k do 
            res <- res*(n-i + 1I)/i
        res

    let binomFloat n k = 
        let mutable res = 1.0
        for i in 1..k do 
            res <- res*(float n - float i + 1.0)/float i
        res

    let divisors n = 
        seq {
            if n = 1 then yield 1 
            else 
                let mutable i = 1
                while (i<<<1) <=n do
                    if n%i = 0 then yield i
                    i <- i + 1
        }

    let rec perms lst= 
        seq {
            if lst |> Seq.isEmpty then yield Seq.empty
            else 
                for el in lst do
                    for rest in perms (lst |> Seq.where (fun e -> not(el.Equals e))) do
                        yield Seq.append (seq {yield el}) rest
        }

    let fact n = 
        let rec factIn x acc = 
            if x = 1 then acc
            else factIn (x-1) x*acc
        factIn n 1

    let rec decPow n k = if k = 1 then n else n* decPow (n-1) (k-1)
    
    let fib = 
        let rec fibIn a b = 
            seq {
                yield a+b
                yield! fibIn b (a+b)
            }
        seq {
            yield! [0I;1I]
            yield! fibIn 0I 1I   
        }