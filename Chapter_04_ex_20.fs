module Chapter_04_ex_20
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote
open Chapter_04_ex_19

// 4.20
// I can't help wondering whether functional programmers in general are:
// 1. lazy - I mean how hard is it to write e.g colorMap map - instead of colMap m ?? ;)
// 2. a bit aloof and reveling in slightly unreadable code ?? ;)
// I think the code gets vastly more readable by using good names:
let colorMap map =
    let areNeighbors map country1 country2 = 
        match map with
        | (countryX, countryY)::countries -> 
            (country1 = countryX && country2 = countryY) || 
            (country2 = countryX  && country1 = countryY)
        | [] -> false
    
    // I'm still not fond of the ' naming here, but haven't come up with
    // something better, yet...
    // What's wrong with the ' ? Its hard to pronounce (doesn't really flow)
    // and it doesn't add any information to what the value is.
    let rec canColorBeExtendedBy map color country =
        match color with
        | [] -> true
        | country'::color' -> 
            not(areNeighbors map country' country) &&
            canColorBeExtendedBy map color' country

    let rec extendColoring map colorings country =
        match colorings with
        | [] -> [[country]]
        | coloring::colorings' -> 
            if canColorBeExtendedBy map coloring country then 
                (country::coloring)::colorings'
            else
                coloring::extendColoring map colorings' country
    
    let addElement x ys = if isMember x ys then ys else x::ys

    let rec countries = function
        | [] -> []
        | (country1, country2)::rest -> addElement country1 (addElement country2 (countries rest))

    let rec colorCountries map = function
        | [] -> []
        | country::rest -> extendColoring map (colorCountries map rest) country

    colorCountries map (countries map)
                                

