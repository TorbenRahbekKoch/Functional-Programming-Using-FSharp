﻿module Chapter_04_ex_15
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.15 revrev
let revrev listOfList =
    let rec reverseList list reversedList =
        match list with 
        | [] -> []
        | x::xs -> reverseList xs ((reverseList x [])::reversedList)
    reverseList listOfList []

[<TestFixture>]
type ``Chapter_04_ex_15_Tests``() = 
    [<Test>]
    member x.``4.15 revrev``() = 
        test <@ revrev [[1;2;3];[4;5;6]] = [[6;5;4];[3;2;1]] @>

