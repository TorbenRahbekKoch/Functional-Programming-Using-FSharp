module Chapter_03_ex_1_Tests
open System
open NUnit.Framework
open FsUnit
open Chapter_03_ex_1

[<TestFixture>]
type ``Chapter_03_Tests``() = 

    [<Test>] 
    member x.``3.1 triple - no custom compare``() = 
        (10, 10, "AM") < (10, 11, "AM") |> should equal true
        (10, 10, "PM") < (10, 11, "AM") |> should equal true // counter-intuitive ;)

        (10, 10, "AM") < (10, 10, "PM") |> should equal true
        (10, 10, "PM") < (10, 11, "PM") |> should equal true

        (11, 59, "AM") < (1, 15, "PM") |> should equal false // counter-intuitive
        (11, 00, "AM") < (1, 15, "AM") |> should equal false

        (11, 59, "AM") < (11, 59, "AM") |> should equal false

    [<Test>] 
    member x.``3.1 triple - custom compare``() = 
        (10, 10, "AM") &< (10, 11, "AM") |> should equal true
        (10, 10, "PM") &< (10, 11, "AM") |> should equal false

        (10, 10, "AM") &< (10, 10, "PM") |> should equal true
        (10, 10, "PM") &< (10, 11, "PM") |> should equal true

        (11, 59, "AM") &< (1, 15, "PM") |> should equal true
        (11, 00, "AM") &< (1, 15, "AM") |> should equal false

        (11, 59, "AM") &< (11, 59, "AM") |> should equal false

    [<Test>] 
    member x.``3.1 record``() = 
        { AmPm = AM; Hour=10; Minute=10 } < { AmPm = AM; Hour=10; Minute=11 } |> should equal true
        { AmPm = PM; Hour=10; Minute=10 } < { AmPm = AM; Hour=10; Minute=11 } |> should equal false

        { AmPm = AM; Hour=10; Minute=10 } < { AmPm = PM; Hour=10; Minute=10 } |> should equal true
        { AmPm = PM; Hour=10; Minute=10 } < { AmPm = PM; Hour=10; Minute=11 } |> should equal true

        { AmPm = AM; Hour=11; Minute=59 } < { AmPm = PM; Hour=1; Minute=15 } |> should equal true
        { AmPm = AM; Hour=11; Minute=00 } < { AmPm = AM; Hour=1; Minute=15 } |> should equal false

        { AmPm = AM; Hour=11; Minute=59 } < { AmPm = AM; Hour=11; Minute=59 } |> should equal false            