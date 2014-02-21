module Chapter_04_ex_18
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.18 
// The type is: ('a -> 'a) ->    'a list     -> 'a list
//                   g        implicit param     result
let rec f g = function
    | [] -> []
    | x::xs -> g x :: f (fun y -> g(g y)) xs

// f g [x0;x1;x2;....;xn-1] = ???
// [g x0; g(g x1);  g(g(g x2))... ??