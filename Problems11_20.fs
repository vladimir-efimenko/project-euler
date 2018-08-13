module Problems11_20
    open System 
    open Problems 

    let problem11 adjLength (str:string) =
        let size = 20
        let strData = str.Split(' ') |> Array.map (fun s -> s.toBigInt)
        let data = Array2D.init size size (fun i j -> strData.[i*size+j]) 

        let getDown i j = 
            if i + adjLength-1 >= size then 0I
            else 
                let mutable prod = 1I
                for k in 1..adjLength do
                    prod <- prod*data.[i + k - 1, j]
                prod

        let getRight i j = 
            if j + adjLength-1 >= size then 0I
            else 
                let mutable prod = 1I
                for k in 1..adjLength do
                    prod <- prod*data.[i, j + k - 1]
                prod

        let getDiag1 i j = 
            if (j + adjLength-1) >= size || (i+adjLength-1) >= size then 0I
            else 
                let mutable prod = 1I
                for k in 1..adjLength do
                    prod <- prod*data.[i + k - 1, j + k - 1]
                prod

        let getDiag2 i j = 
            if (j - adjLength+1) < 0 || (i+adjLength-1) >= size then 0I
            else 
                let mutable prod = 1I
                for k in 1..adjLength do
                    prod <- prod*data.[i + k - 1, j - k + 1]
                prod

        let mutable product = 0I

        for i in 0..size-1 do
            for j in 0..size-1 do
                let down = getDown i j
                let r = getRight i j
                let diag = getDiag1 i j
                let diag2 = getDiag2 i j
                let max = [down;r;diag;diag2] |> Seq.max 
                if max > product then product <- max

        product

    let problem12() = 
        let triNums = 
            let rec triangleNums n k = 
                seq {
                    yield n + k
                    yield! triangleNums (n+k) (k+1)
                }
            triangleNums 0 1

        let divCount x = 
            let mutable total = 1
            for i in 1..x/2 do
                if x%i = 0 then total <- total + 1
            total

        triNums |> Seq.skipWhile (fun x -> divCount x <= 500) |> Seq.take 1

    let problem13 (str:string) = 
        let sum = str.Replace("\r\n", " ").Split(' ') |> Seq.map (fun s -> s.toBigInt) |> Seq.sum
        sum.ToString() |> Seq.take 10 |> Seq.fold (fun s c -> s + string c) ""

    let rec collatzSeq n = 
        seq {
            if n = 1I then 
                yield 1I
            else 
                yield n
                if Numeric.isEven n then 
                    yield! collatzSeq (n/2I) 
                else 
                    yield! collatzSeq (3I*n+1I)
        }

    let problem14() = 
        let comp n = 
            let l = collatzSeq n |> Seq.length
            n,l
        seq { for n in 1I..999999I -> comp n } |> Seq.maxBy snd

    let problem15 n = Numeric.binom (2I*n) n

    let problem16() = (2I**1000).ToString() |> Seq.fold (fun s c -> s + c.toBigInt) 0I

    let problem17() = 
        let oneToNine = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight";"nine"]
        let tenToNineteen = ["ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"]
        let tenths = ["twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"]
        
        let keysLength (lst:string list) = lst |> Seq.sumBy (fun s -> s.Length)

        let oneToNineLength = oneToNine |> keysLength
        let tenToNineteenLength = tenToNineteen |> keysLength
        let twentyToNinetyNineLength = tenths |> Seq.fold (fun s str -> s + str.Length*10 + oneToNineLength) 0

        let mutable res = 0 
        res <- res + oneToNineLength 
        res <-res + tenToNineteenLength
        res <- res + twentyToNinetyNineLength 
        res <- res + 9*"hundred".Length + oneToNineLength + "onethousand".Length

        for i in oneToNine do 
            let hundredAndLength = i.Length + "hundred".Length + "and".Length
            res <- res + hundredAndLength * 9 + oneToNineLength
            res <- res + hundredAndLength * 10 + tenToNineteenLength
            res <- res + hundredAndLength * 80 + twentyToNinetyNineLength
        res

    let problem18_67 (triangleString:string) =
        let getArray = 
            let arr = triangleString.Split([|Environment.NewLine|], StringSplitOptions.None) in
            [|for i in 0..arr.Length - 1 do 
                yield arr.[i].Split(' ') |> Seq.map (fun s -> s.toInt) |> Seq.toArray |]

        let data = getArray
        let sums = new System.Collections.Generic.Dictionary<int*int, int>()

        sums.Add((0,0), data.[0].[0])

        for i = 1 to data.Length - 1 do
            for j = 0 to i do
                let sum = 
                    if j = 0 then sums.[i - 1, 0] + data.[i].[j]
                    elif j = i then sums.[i - 1, j - 1] + data.[i].[j]
                    else Math.Max(sums.[i - 1, j - 1], sums.[i - 1, j]) + data.[i].[j]
                sums.Add((i, j), sum)

        let res = sums |> Seq.where (fun pair -> fst (pair.Key) = data.Length - 1) |> Seq.maxBy (fun pair -> pair.Value)
        res.Value

    let problem19() = 
        let mutable sundays = 0
        let mutable day = new DateTime(1901, 1, 1)
        let last = new DateTime(2000,12,31)
        while day <= last do 
            if day.DayOfWeek = DayOfWeek.Sunday then sundays <- sundays + 1
            day <- day.AddMonths(1)
        sundays

    let problem20() = 
        let fact n = 
            let rec fact x a = 
                match x with 
                    _ when x < 2I -> a 
                    | _ -> fact (x-1I) a*x
            fact n 1I
        (fact 100I).ToString() |> Seq.fold (fun s c -> s + c.toInt) 0