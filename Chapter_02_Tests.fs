module Chapter_02_Tests
open NUnit.Framework
open FsUnit
open Chapter_02

[<TestFixture>]
type ``Chapter_02_Tests``() = 

    [<Test>] 
    member x.``2.1``() = 
        f 24 |> should equal true
        f 27 |> should equal true
        f 29 |> should equal false

    member x.``powTester`` powf =
        powf "a" -1 |> should equal "" // I've chosen to define it like this
        powf "a" 0 |> should equal ""
        powf "a" 1 |> should equal "a"
        powf "a" 2 |> should equal "aa"
        powf "a" 42 |> should equal "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        powf "" 0 |> should equal ""
        powf "" 1 |> should equal ""
        powf "" 2 |> should equal ""
        powf "" 42 |> should equal ""

    [<Test>]
    member x.``2.2_recursive``() =
        x.powTester pow

    [<Test>]
    member x.``2.2_stringBuilder``() =
        x.powTester powWithBuilder

    [<Test>]
    member x.``2.2_stringConcat``() =
        x.powTester powWithStringConcat
