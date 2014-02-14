module Chapter_04_ex_16
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.16 - 1.
// The type is int*int list -> int list
let rec f = function
    | (x, []) -> []
    | (x, y::ys) -> (x+y)::f(x-1, ys)

// f(x, [y0; y1; y2; y3; ...; yn-1] = [x+y0; x-1+y1; x-2+y2; x-3+y3;...;x-(n-1)+yn-1]

// 4.16 - 2.
// The type is 'a*'a list -> 'a*'a list
let rec g = function
    | [] -> []
    | (x, y)::s -> (x, y)::(y, x)::g s


// 4.16 - 3.
// The type is 'a list -> 'a list
let rec h = function
    | [] -> []
    | x::xs -> x::(h xs)@[x]
