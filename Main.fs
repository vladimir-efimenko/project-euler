open System
open System.Net

type A() = 
    member a.Foo = "Foo"

type B() = 
    inherit A()
    member b.Foo = "base" + base.Foo + " derived Bar!"

type ComputationWithOption() = 
    member this.Bind(x, f) = match x with | Some a -> f a | None -> None
    member this.Return(x) =  Some x
    member this.ReturnFrom(x) = x

let strToInt str:int Option = try Some(Int32.Parse str) with _ -> None

[<EntryPoint>]
let main args =

    let time f = 
        let start = DateTime.Now
        let x = f()
        (DateTime.Now - start)

    let warmUP = for i in 1..10 do ()

    B().Foo |> printfn "%A"
    (B() :> A).Foo |> printfn "%A"

    let parse = ComputationWithOption()

    let stringAddWorkflow x y z = 
        parse { 
            let! a = strToInt x
            let! b = strToInt y
            let! c = strToInt z
            return a+b+c
        }

    let good = stringAddWorkflow "12" "3" "2"
    let bad = stringAddWorkflow "12" "xyz" "2"

    printfn "%A " ([1..10] |> List.map Probability.atLeastOne)

    printfn "Running time: %A" (time (fun () -> printfn "%A " (Problems21_30.problem100()) ))

    0