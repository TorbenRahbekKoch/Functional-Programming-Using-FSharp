module Chapter_09_ex_5
    #if INTERACTIVE
    #r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
    #r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
    #endif    
    open System
    open System.Globalization
    open NUnit.Framework
    open Swensen.Unquote

 
    // 9.5
    // The factorial function from page 206;
    let rec factA (n, m) = 
        match (n, m) with
        | (0, m) -> m
        | (n, m) -> factA(n-1, n*m)

    let factC n =
        let rec factC' n c =
            if n = 1 then c 1
            else factC' (n-1) (fun res -> c(res * n))
        factC' n id

    // When passing a continuation you pass a function that 1: captures a value for the current
    // recursion level and 2: can compute the result given a value when the recursion has happened.
     
// Runtime comparison

    let xs16 = List.init 1000000 (fun i -> 16)
    for i in xs16 do let _ = factA (i,1) in ()  // Real: 00:00:00.027, CPU: 00:00:00.027, GC gen0: 0, gen1: 0

    for i in xs16 do let _ = factC i in ()      // Real: 00:00:00.686, CPU: 00:00:00.755, GC gen0: 56, gen1: 0

    [<TestFixture>]
    type ``Chapter 09 exercise 5 Tests``() = 
        [<Test>]
        member x.``9.5 Factorial - continuation passing style``() =    
            test <@ factC 2 = 2 @>
            test <@ factC 3 = 6 @>
            test <@ factC 4 = 24 @>
            test <@ factC 5 = 120 @>

