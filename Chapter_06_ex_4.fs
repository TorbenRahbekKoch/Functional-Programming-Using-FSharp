module Chapter_06_ex_4
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

type BinTree<'a,'b> =
     | Leaf of 'a
     | Node of BinTree<'a,'b> * 'b * BinTree<'a,'b>;;

// 6.4 1. leafValues
let leafValues tree =
    let rec collectLeafValues tree collectedValues =
        match tree with
        | Leaf(leaf)                   -> leaf::collectedValues
        | Node(leftTree,_, rightTree)  -> (collectLeafValues leftTree collectedValues) 
                                        @ (collectLeafValues rightTree collectedValues) 

    collectLeafValues tree []
    |> Set.ofList

// 6.4 2. nodeValues
let nodeValues tree =
    let rec collectNodeValues tree collectedValues =
        match tree with
        | Leaf(_)                   -> collectedValues
        | Node(left, value, right)  -> value::(collectNodeValues left collectedValues)
                                     @ (collectNodeValues right collectedValues)
    collectNodeValues tree []
    |> Set.ofList

// 6.4 3. values
let values tree =
    let rec collectValues tree collectedLeafValues collectedNodeValues =
        match tree with
        | Leaf(leaf)                -> (leaf::collectedLeafValues, collectedNodeValues)
        | Node(left, value, right)  -> 
            let leftValues = collectValues left collectedLeafValues collectedNodeValues
            let rightValues = collectValues right collectedLeafValues collectedNodeValues
            ((fst leftValues)@(fst rightValues), value::(snd leftValues)@(snd(rightValues)))
    let leafs, nodes = collectValues tree [] []
    (leafs |> Set.ofList, nodes |> Set.ofList)

[<TestFixture>]
type ``Chapter 06 exercise 4 Tests``() = 
    [<Test>]
    member x.``6.4 1. leafValues``() = 
        test <@
                let tree = Node(
                                 Node(
                                   Leaf(1), 
                                   "a", 
                                   Node(
                                     Leaf(2), 
                                     "b", 
                                     Leaf(3)
                                   )
                                 ),
                                 "c",
                                 Leaf(3)
                               )
                leafValues tree = Set[1;2;3]
             @>

    [<Test>]
    member x.``6.4 2. nodeValues``() = 
        test <@
                let tree = Node(
                                 Node(
                                   Leaf(1), 
                                   "a", 
                                   Node(
                                     Leaf(2), 
                                     "b", 
                                     Leaf(3)
                                   )
                                 ),
                                 "b",
                                 Leaf(3)
                               )
                nodeValues tree = Set["a";"b"]
             @>

    [<Test>]
    member x.``6.4 2. values``() = 
        test <@
                let tree = Node(
                                 Node(
                                   Leaf(1), 
                                   "a", 
                                   Node(
                                     Leaf(2), 
                                     "b", 
                                     Leaf(3)
                                   )
                                 ),
                                 "b",
                                 Leaf(3)
                               )
                values tree = (Set[1;2;3], Set["a";"b"])
             @>
