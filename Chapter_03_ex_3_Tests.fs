module Chapter_03_ex_3_Tests
open System
open NUnit.Framework
open FsUnit
open Chapter_03_ex_3

[<TestFixture>]
type ``Chapter_03_ex_3_Tests``() = 
    [<Test>]
    member x.``3.3 add``() = 
        (1.0, 1.0) .+ (2.0, 2.0) |> should equal (3.0, 3.0)

    [<Test>]
    member x.``3.3 mul``() = 
        (1.0, 1.0) .* (2.0, 2.0) |> should equal (0.0, 4.0)

    [<Test>]
    member x.``3.3 sub``() = 
        (1.0, 1.0) .- (2.0, 2.0) |> should equal (-1.0, -1.0)

    [<Test>]
    member x.``3.3 div``() = 
        (1.0, 1.0) ./ (2.0, 2.0) |> should equal (0.5, 0.0)

