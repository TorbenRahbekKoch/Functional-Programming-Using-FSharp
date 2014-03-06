module Chapter_05_ex_2
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 5.3 conditional sum using foldBack
let sum predicate items =
    List.foldBack (fun item currentSum ->
                    match predicate item with
                    | false -> currentSum 
                    | true -> item + currentSum
                  )
                  items
                  0

test <@ (sum (fun x -> x > 0) [-1;0;1]) = 1 @> 
test <@ (sum (fun x -> x < 0) [-1;0;1]) = -1 @> 

[<TestFixture>]
type ``Chapter_05_ex 3_Tests``() = 
    [<Test>]
    member x.``5.3 conditional sum using foldBack``() = 
        test <@ (sum (fun x -> x > 0) [-1;0;1]) = 1 @> 
        test <@ (sum (fun x -> x < 0) [-1;0;1]) = -1 @> 
