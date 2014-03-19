module chapter_08_ex1
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

let mutable x = 1
let mutable y = (x,2)
let z = y
x <- 7

// I would have expected y to be (7, 2) - that is I would have expected it to be pointing
// to (x, 2). But the expression (x,2) uses the ContentsOf operator (page 176) on x, since it's
// a right side expression.
test <@ fst y = 1 @>

//      Environment     Store
// ~>   x |-> loc1      loc1: 1
// ~>   y |-> loc2      loc2: (1, 2)
// ~>   z |-> loc2
// ~>   x |-> loc1      loc1: 7
