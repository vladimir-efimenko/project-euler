module Problems
    type System.Char with 
        member this.toBigInt = this.ToString() |> bigint.Parse
        member this.toInt = this.ToString() |> System.Int32.Parse
    
    type System.String with 
        member this.toInt = this |> System.Int32.Parse
        member this.toBigInt = this |> bigint.Parse