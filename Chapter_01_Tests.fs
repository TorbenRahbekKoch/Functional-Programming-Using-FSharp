module Chapter_01_Tests
open NUnit.Framework
open FsUnit
open Chapter_01

[<TestFixture>]
type ``Chapter 01 Tests``() = 

    [<Test>] 
    member x.``Exercise 1.1``() = 
        g 7 |> should equal  11

    [<Test>]
    member x.``Exercise 1.2``() =
        h1 1. 1. |> should (equalWithin 0.01) (System.Math.Sqrt(2.))
        h2 1. 1. |> should (equalWithin 0.01) (System.Math.Sqrt(2.))

    [<Test>]
    member x.``Exercise 1.3 gf``() =
        gf 7 |> should equal  11

    [<Test>]
    member x.``Exercise 1.3 gh``() =
        hf1 (1., 1.) |> should (equalWithin 0.01) (System.Math.Sqrt(2.))
        hf1 (2., 2.) |> should (equalWithin 0.01) (System.Math.Sqrt(8.))
        hf2 (1., 1.) |> should (equalWithin 0.01) (System.Math.Sqrt(2.))
        hf2 (2., 2.) |> should (equalWithin 0.01) (System.Math.Sqrt(8.))

    [<Test>]
    member x.``Exercise 1.4``() =
        f14 1 |> should equal 1
        f14 2 |> should equal 3
        f14 3 |> should equal 6
        f14 4 |> should equal 10

    [<Test>]
    member x.``Exercise 1.5``() =
        fib 0 |> should equal 0
        fib 1 |> should equal 1
        fib 2 |> should equal 1
        fib 3 |> should equal 2
        fib 4 |> should equal 3
        fib 5 |> should equal 5
        fib 6 |> should equal 8
        fib 7 |> should equal 13

    [<Test>]
    member x.``Exercise 1.6``() =
        sum(2, 2) |> should equal 9
        sum(3, 2) |> should equal 12

