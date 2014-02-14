module Chapter_04_ex_17
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4.17
// The type is ('a -> bool) -> 'a list -> 'a list
let rec p q = function
    | [] -> []
    | x::xs -> let ys = p q xs
               if q x then x::ys else ys @[x]

p (fun x -> x % 2 = 1) [1;2;3;4] 
|> ignore
// p q [x0;x1;x2;xn-1] ???
