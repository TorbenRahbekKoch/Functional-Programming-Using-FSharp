module Chapter_04_ex_7
open NUnit.Framework
open FsUnit

// 4.7 - filter and length
let multiplicity x xs =
    xs 
    |> List.filter(fun item -> item = x)
    |> List.length

[<TestFixture>]
type ``Chapter_04_ex_7_Tests``() = 
    [<Test>]
    member x.``4.7 filter and length``() = 
        multiplicity 1 [] |> should equal 0
        multiplicity 2 [2] |> should equal 1
        multiplicity 1 [1] |> should equal 1
        multiplicity 2 [2;4] |> should equal 1
        multiplicity 1 [1;2;1;4;5;6] |> should equal 2
