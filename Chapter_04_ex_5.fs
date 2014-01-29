module Chapter_04_ex_5
open NUnit.Framework
open FsUnit

// 4.5 - simple filtering
// Note that the numbering is zero-based, so the first item in the list is
// considered being in an even position.
let rmodd numbers =
    numbers 
    |> List.mapi(fun index item -> (index, item))
    |> List.filter(fun (index, item) -> index % 2 = 0)
    |> List.map(fun (index, item) -> item)



[<TestFixture>]
type ``Chapter_04_ex_5_Tests``() = 
    [<Test>]
    member x.``4.5 mapping and filtering``() = 
        rmodd [] |> should equal []
        rmodd [2] |> should equal [2]
        rmodd [2;4] |> should equal [2]
        rmodd [2;4;6;8;10;12;14;16;18;20] |> should equal [2;6;10;14;18] 
