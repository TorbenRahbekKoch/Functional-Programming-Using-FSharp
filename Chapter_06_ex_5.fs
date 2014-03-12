module Chapter_06_ex_5
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

// 6.5 AncestorTree
type AncestorTree = 
    | Unspecified
    | Info of AncestorTree * string * AncestorTree

// Grandfather1 Grandmother1  GrandFather2 unknown/unspec
//         father                      mother
//                    child

let tree = Info(
             Info(
                Info(
                  Unspecified,
                  "Grandfather1",
                  Unspecified),
                "Father",
                Info(
                  Unspecified,
                  "Grandmother1",
                  Unspecified)
              ),
              "Child",
              Info(
                Info(
                  Unspecified,
                  "Grandfather2",
                  Unspecified),
                "Mother",
                Unspecified)
              )

let maleAncestors tree = 
    let rec collectMaleAncestors tree isFatherTree collectedValues =
        match tree, isFatherTree with
        | Unspecified,_                    -> collectedValues
        | Info(fathers,male,mothers), true -> male::(collectMaleAncestors fathers true collectedValues)
                                            @ (collectMaleAncestors mothers false collectedValues)
        | Info(fathers,_,mothers), false   -> (collectMaleAncestors fathers true collectedValues)
                                            @ (collectMaleAncestors mothers false collectedValues)

    collectMaleAncestors tree false []

test <@ maleAncestors tree = ["Father"; "Grandfather1"] @>
