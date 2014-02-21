module Chapter_04_ex_10
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/FsUnit.1.2.1.0/lib/net40/FsUnit.NUnit.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open FsUnit
open Swensen.Unquote

let rec prefix list1 list2 =
    match list1, list2 with
    | [],[] -> false
    | x1::[], [] -> false // list1 longer than list2 is per definition not a match
    | x1::[], x2::[] -> x1 = x2
    | x1::xs1, x2::[] -> false // list1 longer than list2 is per definition not a match// failwith "test"//x1 = x2 && prefix xs1 [] 
    | x1::[], x2::xs2 -> x1 = x2 
    | x1::xs1, x2::xs2 -> x1 = x2 && prefix xs1 xs2 
    | _ -> failwith "Incomplete match"

[<TestFixture>]
type ``Chapter_04_ex_10_Tests``() = 
    [<Test>]
    member x.``4.10 recursive``() = 
        prefix [] [] |> should equal false
        prefix [1] [1] |> should equal true
        test <@ prefix [1;2] [1;2] = true  @>
        test <@ prefix [1] [1;2] = true  @>
        test <@ prefix [1] [1;2;3] = true  @>
        test <@ prefix [1;2] [1;2;3] = true  @>
        test <@ prefix [0] [1;2] = false @>
        test <@ prefix [1;2] [1] = false @>


