module Chapter_04_ex_10
open NUnit.Framework
open FsUnit

let rec prefix list1 list2 =
    match list1, list2 with
    | [],[] -> false
    | x1::[], x2::[] -> x1 = x2
    | x1::xs1, x2::[] -> x1 = x2 && prefix xs1 [] 
    | x1::[], x2::xs2 -> x1 = x2 && prefix [] xs2
    | x1::xs1, x2::xs2 -> x1 = x2 && prefix xs1 xs2 
    //| _ -> false

[<TestFixture>]
type ``Chapter_04_ex_10_Tests``() = 
    [<Test>]
    member x.``4.10 recursive``() = 
//        prefix [] [] |> should equal false
//        prefix [1] [1] |> should equal true
        prefix [1] [1;2] |> should equal true
        prefix [0] [1;2] |> should equal false
        prefix [1;2] [1] |> should equal false

