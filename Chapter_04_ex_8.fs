module Chapter_04_ex_8
open NUnit.Framework
open FsUnit

// 4.8  recursive (not tail-recursive)
let rec split items = 
    match items with
    | [] -> ([],[])
    | [x0] -> ([x0],[])
    | [x0;x1] -> ([x0],[x1])
    | x0::x1::xs -> let (r1, r2) = split xs
                    (x0::r1, x1::r2)

[<TestFixture>]
type ``Chapter_04_ex_8_Tests``() = 
    [<Test>]
    member x.``4.8 recursive``() = 
        split [] |> should equal ([], [])

        // This one is hard to test, since the value ([2],[]) has type int list*'a list
        // split [2] |> should equal ([2],[])
        split [2;4] |> should equal ([2],[4])

        split [1;2;3;4;5] |> should equal ([1;3;5],[2;4])
        split [1;2;3;4;5;6] |> should equal ([1;3;5],[2;4;6])
