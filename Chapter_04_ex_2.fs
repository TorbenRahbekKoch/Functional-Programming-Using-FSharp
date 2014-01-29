module Chapter_04_ex_2
open NUnit.Framework
open FsUnit

// 4.2
// Using list comprehension and rev
let downto1a n =
    List.rev [for i in 1..n do yield i]
    
// Using list comprehension without rev
let downto1b n =
    [for i in 1..n do yield n-i+1]
    
// Using tail-recursion (I hope)
// Again, not too crazy about the ' naming. Eventually I will come up with 
// something I like better.
let downto1c n =
    let rec downto1' n' l =
        match n' with
        | 0 -> l
        | x -> downto1' (x-1) ((n-x+1)::l)
    downto1' n []


[<TestFixture>]
type ``Chapter_04_ex_2_Tests``() = 
    [<Test>]
    member x.``4.2 list comprehension with List.rev``() = 
        downto1a 10 |> should equal [10;9;8;7;6;5;4;3;2;1]

    [<Test>]
    member x.``4.2 list comprehension without List.rev``() = 
        downto1b 10 |> should equal [10;9;8;7;6;5;4;3;2;1] 

    [<Test>]
    member x.``4.2 tail recursion``() = 
        downto1c 10 |> should equal [10;9;8;7;6;5;4;3;2;1] 

