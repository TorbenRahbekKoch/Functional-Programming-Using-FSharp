module Chapter_04_ex_11
open System
open NUnit.Framework
open FsUnit
// #r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
open Swensen.Unquote

// 4.11 bonus - check whether the list is actually a weak ascending list
let checkWeakAscent weakList =
    let rec checkWeakAscent' weakList current =
        match weakList with
        | [] -> true
        | x::[] -> current <= x
        | x1::xs -> (current <= x1) && checkWeakAscent' xs x1
    checkWeakAscent' weakList Int32.MinValue

// 4.11 - 1. Does not check the list for weak ascending list criteria
let rec count weakList item =
    match weakList with
    | [] -> 0
    | x::[] -> if x = item then 1 else 0
    | x::xs -> (if x = item then 1 else 0) + count xs item
    
// 4.11 - 2. Insert
let rec insert weakList item =
    match weakList with
    | [] -> [item]
    | x::[] when item <= x -> [item] @ [x]
    | x::[] when item > x -> [x] @ [item]
    | x::xs when item <= x -> item::[x] @ xs
    | x::xs when item > x -> x::(insert xs item)
    | _ -> failwith "Incomplete match on %A" weakList

[<TestFixture>]
type ``Chapter_04_ex_11_Tests``() = 
    [<Test>]
    member x.``4.11 bonus checkWeakAscent``() = 
        checkWeakAscent [] |> should equal true
        checkWeakAscent [1] |> should equal true
        checkWeakAscent [1;1] |> should equal true
        checkWeakAscent [1;2] |> should equal true
        checkWeakAscent [1;1;1] |> should equal true
        checkWeakAscent [1;2;1] |> should equal false
        checkWeakAscent [1;2;2] |> should equal true
        checkWeakAscent [1;2;3] |> should equal true
        checkWeakAscent [2;2;3] |> should equal true
        checkWeakAscent [3;2] |> should equal false
        checkWeakAscent [3;2;3] |> should equal false

    [<Test>]
    member x.``4.11 1. count recursive``() = 
        count [] 1 |> should equal 0
        count [1] 0 |> should equal 0
        count [1] 1 |> should equal 1
        count [1;2;3] 1 |> should equal 1
        count [1;1;3] 1 |> should equal 2
        
    [<Test>]
    member x.``4.11 2. insert``() = 
        test <@ insert [] 1 = [1] @>
        test <@ insert [1] 0 = [0;1] @>
        test <@ insert [1] 1 = [1;1] @>
        test <@ insert [1] 2 = [1;2] @>
        test <@ insert [1;2;3] 1 = [1;1;2;3] @>
        test <@ insert [1;1;3] 4 = [1;1;3;4] @>
        test <@ insert [1;1;3] 2 = [1;1;2;3] @>
        
