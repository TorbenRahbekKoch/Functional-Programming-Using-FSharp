module Chapter_03_ex_4_Tests
open System
open NUnit.Framework
open FsUnit
open Chapter_03_ex_4

[<TestFixture>]
type ``Chapter_03_ex_4_Tests``() = 
    [<Test>]
    member x.``3.4 mirrorX tuple``() = 
        mirrorX (1, 2) |> should equal (-1, -2)

    [<Test>]
    member x.``3.4 mirrorX record``() = 
        mirrorXRec {a=1.0;b=2.0} |> should equal {a= -1.0; b= -2.0}

    [<Test>]
    member x.``3.4 mirrorY tuple``() = 
        mirrorY (1, 2) |> should equal (-1, 2)

    [<Test>]
    member x.``3.4 mirrorY record``() = 
        mirrorYRec {a=1.0;b=2.0} |> should equal {a= -1.0; b= 2.0}
