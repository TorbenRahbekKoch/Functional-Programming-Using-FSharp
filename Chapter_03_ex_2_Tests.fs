module Chapter_03_ex_2_Tests
open System
open NUnit.Framework
open FsUnit
open Chapter_03_ex_2

[<TestFixture>]
type ``Chapter_03_ex_2_Tests``() = 
    [<Test>]
    member x.``3.2 triple add``() = 
        (1, 0, 0) &+ (1, 0, 0) |> should equal (2, 0, 0)
        (1, 1, 1) &+ (1, 1, 1) |> should equal (2, 2, 2)
        (1, 1, 11) &+ (1, 1, 1) |> should equal (2, 3, 0)
        (1, 19, 11) &+ (1, 1, 1) |> should equal (3, 1, 0)

    [<Test>]
    member x.``3.2 triple sub``() = 
        (1, 0, 0) &- (1, 0, 0) |> should equal (0, 0, 0)
        (1, 1, 1) &- (1, 1, 1) |> should equal (0, 0, 0)
        (1, 1, 11) &- (1, 1, 1) |> should equal (0, 0, 10)
        (1, 19, 11) &- (1, 1, 1) |> should equal (0, 18, 10)

        (3, 1, 3) &- (1, 5, 6) |> should equal (1, 15, 9)

    [<Test>]
    member x.``3.2 record add``() = 
        { pound=1; shilling=0; pence=0} |+ { pound=1; shilling=0; pence=0} |> should equal { pound=2; shilling=0; pence=0}
        { pound=1; shilling=1; pence=1} |+ { pound=1; shilling=1; pence=1} |> should equal { pound=2; shilling=2; pence=2}
        { pound=1; shilling=1; pence=11} |+ { pound=1; shilling=1; pence=1} |> should equal { pound=2; shilling=3; pence=0}
        { pound=1; shilling=19; pence=11} |+ { pound=1; shilling=1; pence=1} |> should equal { pound=3; shilling=1; pence=0}

    [<Test>]
    member x.``3.2 record sub``() = 
        { pound=1; shilling=0; pence=0} |- { pound=1; shilling=0; pence=0} |> should equal { pound=0; shilling=0; pence=0}
        { pound=1; shilling=1; pence=1} |- { pound=1; shilling=1; pence=1} |> should equal { pound=0; shilling=0; pence=0}
        { pound=1; shilling=1; pence=11} |- { pound=1; shilling=1; pence=1} |> should equal { pound=0; shilling=0; pence=10}
        { pound=1; shilling=19; pence=11} |- { pound=1; shilling=1; pence=1} |> should equal { pound=0; shilling=18; pence=10}
        { pound=3; shilling=1; pence=3} |- { pound=1; shilling=5; pence=6} |> should equal { pound=1; shilling=15; pence=9}

// 3*12 + 1*20 + 3