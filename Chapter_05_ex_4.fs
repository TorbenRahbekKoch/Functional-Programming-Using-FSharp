module Chapter_05_ex_4
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 5.4 downto1
let downto1 f n e =
    if n > 0 then
        let items = [1..n]
        List.foldBack f items e
    else
        e

let fact n =
    downto1 (*) n 1


[<TestFixture>]
type ``Chapter 05 exercise 4 Tests``() = 
    [<Test>]
    member x.``5.4 downto1 using foldBack``() = 
        test <@ downto1 (+) 3 0 = 6 @>
        test <@ downto1 (+) 3 5 = 11 @>
        test <@ downto1 (+) -3 5 = 5 @>

    [<Test>]
    member x.``5.4 factorial using downto1``() = 
        test <@ fact 0 = 1 @>
        test <@ fact 1 = 1 @>
        test <@ fact 5 = 120 @>
                
