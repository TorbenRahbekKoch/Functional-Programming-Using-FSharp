module Chapter_05_ex_2
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

let foldedRevrev (listOfList: int list list) =
    let reverseInner list =
        List.foldBack (fun item reversedList -> reversedList @ [item]) list []

    List.foldBack (fun item reversedList -> reversedList @ [(reverseInner item)]) listOfList []

[<TestFixture>]
type ``Chapter 05 exercise 2 Tests``() = 
    [<Test>]
    member x.``5.2 foldedRevrev``() = 
        test <@ (foldedRevrev [[1;2;3];[4;5;6]]) = [[6;5;4];[3;2;1]] @>
