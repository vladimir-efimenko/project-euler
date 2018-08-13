module Problems41_50
    open Primes

    let problem47_Int n = 
        let mutable count = 0
        let mutable num = 2

        while count <> n do 
            if factorizeInt num |> SeqUtils.hasExactly n then count <- count + 1
            else count <- 0
            num <- num + 1
        num - n

    let problem47_BigInt n = 
        let mutable count = 0
        let mutable num = 2I

        while count <> n do 
            if factorize num |> SeqUtils.hasExactly n then count <- count + 1
            else count <- 0
            num <- num + 1I
        num - bigint n

    let problem50 lim = 
        let mutable res = 0I
        let mutable prime = 1I
        let primeIter = primesGen.GetEnumerator()
        let mutable primes = List.empty

        while res < lim && primeIter.MoveNext() do 
            prime <- primeIter.Current
            primes <- prime::primes 
            res <- res + prime
        res <- res-prime
        primes <- List.tail primes |> List.rev

        while not(fastPrimeCheck res) do 
            res <- res - Seq.head primes
            primes <- List.tail primes
        res 