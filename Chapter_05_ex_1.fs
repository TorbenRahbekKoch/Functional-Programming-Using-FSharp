module Chapter_05_ex_1
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 5.1
// List.filter: ('a -> bool) -> 'a list -> 'a list
let foldedFilter predicate items =
    List.foldBack (fun item state ->
                    match predicate item with
                    | false -> state 
                    | true -> item::state
                  )
                  items
                  []

[<TestFixture>]
type ``Chapter_05_ex 1_Tests``() = 
    [<Test>]
    member x.``5.1 foldedFilter``() = 
        test <@ (foldedFilter (fun x -> x > 0) [-2;-1;0;1;2]) = [1;2] @>