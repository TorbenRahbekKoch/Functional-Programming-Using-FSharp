module Chapter_09_ex_4
    #if INTERACTIVE
    #r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
    #r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
    #endif    
    open System
    open System.Globalization
    open NUnit.Framework
    open Swensen.Unquote

    // 9.4

    let listLength list = 
        let mutable length = 0
        for item in list do
            length <- length+1
        length

    [<TestFixture>]
    type ``Chapter 09 exercise 4 Tests``() = 
        [<Test>]
        member x.``9.4 Iterative list length``() =    
            test <@ listLength [] = 0 @>
            test <@ listLength [1] = 1 @>
            test <@ listLength [1;2;3;4;5;6] = 6 @>
