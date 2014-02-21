module Chapter_04_ex_19
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 4. 19 
let rec isMember x = function
    | y::ys -> x=y || (isMember x ys)
    | [] -> false

let areNb m c1 c2 = 
    isMember(c1, c2) m || isMember(c2, c1) m

// Well, if isMember(c1, c2) does not find it, it has to traverse the map again
// looking of (c2, c1)

let areNeighbors map country1 country2 = 
    match map with
    | (countryX, countryY)::countries -> 
        (country1 = countryX && country2 = countryY) || 
        (country2 = countryX  && country1 = countryY)
    | [] -> false

// The fully generic isMember cannot do this, since it has no knowledge of the
// data structure it is comparing - is doesn't know that (x,y) = (y,x)

