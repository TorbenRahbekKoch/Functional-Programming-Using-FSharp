module Chapter_04_ex_1
open NUnit.Framework
open FsUnit

// 4.1
// Using list comprehension
let upto_a n =
    [for i in 1..n do yield i]

// Recursive - and if I am correct - its tail-recursive - this will 
// do a stackoverflow, though, for negative values.
// I don't personally like the upto' naming, its hard to read, its
// hard to pronounce - in C# it probably would have been named
// upto_impl, which is just as bad
let rec upto_b n = 
    let rec upto' n l =
        match n with
        | 0 -> l
        | x -> upto' (x-1) (x::l)
    upto' n []

[<TestFixture>]
type ``Chapter_04_ex_1_Tests``() = 
    [<Test>]
    member x.``4.1 list comprehension``() = 
        upto_a 10 |> should equal [1;2;3;4;5;6;7;8;9;10] 

    [<Test>]
    member x.``4.1 recursive``() = 
        upto_b 10 |> should equal [1;2;3;4;5;6;7;8;9;10] 
