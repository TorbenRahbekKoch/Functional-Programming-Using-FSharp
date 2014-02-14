module Chapter_04_ex_13
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.13 - 1. find smallest element
let findSmallestElement list = 
    let rec find (list: int list) (currentSmallest:int) =
        match list with
        | [] -> currentSmallest
        | x::xs -> let newCurrentSmallest = Math.Min(x, currentSmallest) 
                   find xs newCurrentSmallest
    find list Int32.MaxValue 

// 4.13 - 2. delete
let delete (list, itemToDelete) =
    let rec deleteItem list itemToRemove resultList =
        match list with
        | [] -> resultList
        | x::[] when x = itemToRemove -> resultList 
        | x::xs when x = itemToRemove -> resultList @ xs
        | x::xs -> resultList @ [x] @ deleteItem xs itemToRemove resultList
    deleteItem list itemToDelete []

// 4.13 - 3. sort
let sortToWeak list =
    let rec weakSort rest result = 
        match rest with
        | [] -> result
        | x::xs -> let smallestElement = findSmallestElement rest
                   weakSort (delete(rest, smallestElement)) (result @ [smallestElement])
    weakSort list [] 


[<TestFixture>]
type ``Chapter_04_ex_13_Tests``() = 
    [<Test>]
    member x.``4.13.1 find smallest element``() = 
        test <@ findSmallestElement [5;2;8;2;6;2] = 2 @>

    [<Test>]
    member x.``4.13.2 delete element``() = 
        test <@ delete([1;4;3;2;6;7;8;2;2;4;8], 2) = [1;4;3;6;7;8;2;2;4;8] @>

    [<Test>]
    member x.``4.13.3 weak sort``() = 
        test <@ sortToWeak [2;5;8;9;1;2;3] = [1;2;2;3;5;8;9] @>


