module Primes

    let slowPrimeCheck x = 
        if x < 2I then false
        else 
            let rec primei x i = if x%i = 0I then false else (i > x/2I) || primei x (i+1I)
            primei x 2I

    let fastPrimeCheck x = 
        if x < 2I then false
        elif x = 2I || x = 3I then true
        elif x%2I = 0I || x%3I = 0I then false
        else 
            let mutable i = 5I
            let mutable w = 2I
            let mutable brk = false
            while i*i <= x && not brk do 
                if x%i = 0I then 
                    brk <- true
                i <- (i + w)
                w <- (6I - w)
            not brk

    let fastPrimeCheckInt x = 
        if x < 2 then false
        elif x = 2 || x = 3 then true
        elif x%2 = 0 || x%3 = 0 then false
        else 
            let mutable i = 5
            let mutable w = 2
            let mutable brk = false
            while i*i <= x && not brk do 
                if x%i = 0 then 
                    brk <- true
                i <- (i + w)
                w <- (6 - w)
            not brk

    let eratospheneGen = 
        let rec gen p x = 
            seq {
                if p |> Seq.forall (fun y-> x%y <> 0) then 
                    yield x
                    yield! gen ([x] |> Seq.append p) (x+1)
                else 
                    yield! gen p (x+1)
            }
        seq {
            yield 2
            yield! gen [2] 3
        }

    let primesGen = 
        seq {
            let mutable i = 2I
            while true do 
                if fastPrimeCheck i then 
                    yield i
                i <- i + 1I
        }

    let primesGenInt = 
        seq {
            let mutable i = 2
            while true do 
                if fastPrimeCheckInt i then 
                    yield i
                i <- i + 1
        }

    let factorize x = 
        let mutable i = x
        let allPrimes = primesGen.GetEnumerator()
        seq {
            while allPrimes.MoveNext() && i > 1I do 
                let prime = allPrimes.Current
                if i%prime = 0I then 
                    let mutable count = 0
                    while i%prime=0I do
                        i <- i/prime
                        count <- count + 1
                    yield prime, count
        }

    let factorizeInt x = 
        let mutable i = x
        let allPrimes = primesGenInt.GetEnumerator()
        seq {
            while allPrimes.MoveNext() && i > 1 do 
                let prime = allPrimes.Current
                if i%prime = 0 then 
                    let mutable count = 0
                    while i%prime=0 do
                        i <- i/prime
                        count <- count + 1
                    yield prime, count
        }
