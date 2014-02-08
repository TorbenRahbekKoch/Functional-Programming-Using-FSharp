module Chapter_04_ex_12
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.12 sum
let sum (predicate, list)=
    let rec sum' (predicate: int -> bool) (list: int list) (currentSum:int) =
        match list with
        | [] -> currentSum
        | x::xs -> sum' predicate xs currentSum + (if predicate x then x else 0)
    sum' predicate list 0

[<TestFixture>]
type ``Chapter_04_ex_12_Tests``() = 
    [<Test>]
    member x.``4.12 predicate sum``() = 
        test <@ sum ((fun x -> x > 0), [-1;0;1]) = 1 @> 
        test <@ sum ((fun x -> x < 0), [-1;0;1]) = -1 @> 
