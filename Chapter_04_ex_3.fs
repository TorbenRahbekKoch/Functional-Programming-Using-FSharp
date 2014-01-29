module Chapter_04_ex_3
open NUnit.Framework
open FsUnit

// 4.3 - with list comprehension
let evenN n =
    [for x in 2..2..(n*2) do yield x]
//   for x in start..step..end


[<TestFixture>]
type ``Chapter_04_ex_3_Tests``() = 
    [<Test>]
    member x.``4.3 list comprehension with List.rev``() = 
        evenN 10 |> should equal [2;4;6;8;10;12;14;16;18;20]
