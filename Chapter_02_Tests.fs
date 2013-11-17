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


    [<Test>]
    member x.``2.3 isIthChar``() =
        isIthChar ("abcdef", 2, 'c')  |> should equal true
        isIthChar ("abcdef", 2, 'd') |> should equal false
        isIthChar ("abcdef", 17, 'g') |> should equal false

    [<Test>]
    member x.``2.4 occFromIth``() =
        occFromIth ("abcabcabc", -1, 'a') |> should equal 0
        occFromIth ("abcabcabc", 0, 'a') |> should equal 3
        occFromIth ("abcabcabc", 5, 'a') |> should equal 1
        occFromIth ("abcabcabc", 15, 'a') |> should equal 0

    [<Test>]
    member x.``2.5 occInString``() =
        occInString ("abcbcecde", 'a') |> should equal 1
        occInString ("abcbcecde", 'b') |> should equal 2
        occInString ("abcbcecde", 'c') |> should equal 3
        occInString ("abcbcecde", 'e') |> should equal 2
        occInString ("abcbcecde", 'f') |> should equal 0

    [<Test>]
    member x.``2.6 notDivisible``() =
        notDivisible (-2, -5) |> should equal true
        notDivisible (2, 5) |> should equal true
        notDivisible (9, 3) |> should equal false

