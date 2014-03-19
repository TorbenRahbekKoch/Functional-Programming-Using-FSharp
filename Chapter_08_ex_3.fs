module Chapter_08_ex_3
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

                                         // Environment                      // Store
type t1 = { mutable a : int }              
type t2 = { mutable b : int; c : t1 }       
let x = { a = 1 }                       //  x |-> { a |-> loc1 }                loc1 : { a |-> loc2 }, loc2: 1
let y = { b = x.a; c = x}               //  y |-> { b |-> 1; c |-> loc1 }       loc3 : { b |-> loc2 }   
x.a <- 3                                //                                      loc2 : 3

test <@ x.a = 3 @>
test <@ y.b = 1 @>    // First I would have expected y.b to be 3 - continued below...
test <@ y.c.a = 3 @>

// but the assignment b = x.a; uses the implicit ContentsOf operator introduced on page 176.