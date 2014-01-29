module Chapter_04_ex_6
open NUnit.Framework
open FsUnit

// 4.6 - using a filter
let rmeven numbers =
    numbers |> List.filter(fun item -> item % 2 > 0)

[<TestFixture>]
type ``Chapter_04_ex_6_Tests``() = 
    [<Test>]
    member x.``4.6 simple filtering``() = 
        rmeven [] |> should equal []
        rmeven [2] |> should equal []
        rmeven [1] |> should equal [1]
        rmeven [2;4] |> should equal []
        rmeven [1;2;3;4;5;6] |> should equal [1;3;5]
