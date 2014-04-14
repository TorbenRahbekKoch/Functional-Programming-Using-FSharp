module Chapter_09_ex_3
    #if INTERACTIVE
    #r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
    #r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
    #endif    
    open System
    open System.Globalization
    open NUnit.Framework
    open Swensen.Unquote

    // 9.3

    let sum (m, n) =
        let mutable result = m
        for value in 1..n do
            result <- result + (m + value)
        result


    [<TestFixture>]
    type ``Chapter 09 exercise 3 Tests``() = 
        [<Test>]
        member x.``9.3 Iterative sum(m,n)``() =    
            test <@ sum(2,2) = 9@>
            test <@ sum(3, 2) = 12 @>
