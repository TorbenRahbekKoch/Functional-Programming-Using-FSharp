module Chapter_04_ex_15
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.15 revrev
let revrev listOfList =
    let rec reverseInner list reversedList =
        match list with
        | [] -> reversedList
        | x::xs -> reverseInner xs (x::reversedList)

    let rec reverseList list reversedList =
        match list with 
        | [] -> reversedList
        | x::xs -> reverseList xs ((reverseInner x [])::reversedList)
    reverseList listOfList []

test <@ revrev [[1;2;3];[4;5;6]] = [[6;5;4];[3;2;1]] @>
//
//let rec reverseInner list reversedList =
//    match list with
//    | [] -> reversedList
//    | x::xs -> reverseInner xs (x::reversedList)
//
//test <@ revrev [1;2;3]] = [[6;5;4];[3;2;1;3]] @>
//
[<TestFixture>]
type ``Chapter_04_ex_15_Tests``() = 
    [<Test>]
    member x.``4.15 revrev``() = 
        test <@ revrev [[1;2;3];[4;5;6]] = [[6;5;4];[3;2;1]] @>

