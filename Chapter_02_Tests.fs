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

    [<Test>]
    member x.``2.7 1. test``() =
        test(4, 6, 2) |> should equal false
        test(6, 9, 5) |> should equal true

        test_if(4, 6, 2) |> should equal false
        test_if(6, 9, 5) |> should equal true

    [<Test>]
    member x.``2.7 2. prime``() =
        prime 1 |> should equal true
        prime 2 |> should equal true
        prime 3 |> should equal true
        prime 4 |> should equal false
        prime 5 |> should equal true
        prime 6 |> should equal false
        prime 7 |> should equal true
        prime 8 |> should equal false
        prime 9 |> should equal false
        prime 10 |> should equal false
        prime 23 |> should equal true

    [<Test>]
    member x.``2.7 3. nextPrime``() =
        nextPrime 1 |> should equal 2
        nextPrime 2 |> should equal 3
        nextPrime 3 |> should equal 5
        nextPrime 4 |> should equal 5
        nextPrime 5 |> should equal 7
        nextPrime 6 |> should equal 7
        nextPrime 7 |> should equal 11
        nextPrime 8 |> should equal 11
        nextPrime 9 |> should equal 11
        nextPrime 10 |> should equal 11
        nextPrime 23 |> should equal 29

    [<Test>]
    member x.``2.8 binomial coefficients``() =
        bin(0,0) |> should equal 1
        bin(1,0) |> should equal 1
        bin(1,1) |> should equal 1
        bin(2,0) |> should equal 1
        bin(2,1) |> should equal 2
        bin(2,2) |> should equal 1
        bin(3,0) |> should equal 1
        bin(3,1) |> should equal 3
        bin(3,2) |> should equal 3
        bin(3,3) |> should equal 1
        bin(4,0) |> should equal 1
        bin(4,1) |> should equal 4
        bin(4,2) |> should equal 6
        bin(4,3) |> should equal 4
        bin(4,4) |> should equal 1
        bin(5,0) |> should equal 1
        bin(5,1) |> should equal 5
        bin(5,2) |> should equal 10
        bin(5,3) |> should equal 10
        bin(5,4) |> should equal 5
        bin(5,5) |> should equal 1

