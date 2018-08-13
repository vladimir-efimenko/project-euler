module Probability
    open System

    let atLeastOne n = 
        [1..n] 
        |> List.fold (fun s i -> s + Math.Pow(-1.0, float (i+1))*(Numeric.binomFloat n i)/float (Numeric.decPow n i)) 0.