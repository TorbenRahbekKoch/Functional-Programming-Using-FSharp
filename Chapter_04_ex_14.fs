module Chapter_04_ex_14
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.14 - Find smallest int
let findSmallestElement list =
    let rec find (list: int list) (currentSmallest:int) =
        match list with
        | [] -> None
        | x::[] -> Some(currentSmallest) 
        | x::xs -> let newCurrentSmallest = Math.Min(x, currentSmallest) 
                   find xs newCurrentSmallest
    find list Int32.MaxValue 

[<TestFixture>]
type ``Chapter_04_ex_14_Tests``() = 
    [<Test>]
    member x.``4.14 find smallest element option``() = 
        test <@ findSmallestElement [5;2;8;2;6;2] = Some(2) @>
        test <@ findSmallestElement [] = None @>
