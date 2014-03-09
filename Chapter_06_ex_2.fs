module Chapter_06_ex_2
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

// 6.2 post fix form
type Fexpr = | Const of float
             | X // What is this doing??
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr

let rec toPostFix expression =
    match expression with
    | Const(value)      -> String.Format(CultureInfo.InvariantCulture, "{0:0.0######}", value)
    | Add(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " +"
    | Sub(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " -"
    | Mul(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " *"
    | Div(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " /"
    | Sin(expr)         -> toPostFix(expr) + " sin"
    | Cos(expr)         -> toPostFix(expr) + " cos"
    | Log(expr)         -> toPostFix(expr) + " log"
    | Exp(expr)         -> toPostFix(expr) + " exp"
    | expr              -> toPostFix(expr)

[<TestFixture>]
type ``Chapter 06 exercise 2 Tests``() = 
    [<Test>]
    member x.``6.2 format expressions in postfix form``() = 
        test <@ toPostFix(Add(Const(7.0), Const(14.0))) = "7.0 14.0 +" @>
        test <@ toPostFix(Mul(
                           Add(Const(7.0), Const(14.0)), 
                           Sub(Const(6.0), Const(12.0)))) 
                 = "7.0 14.0 + 6.0 12.0 - *" 
             @>
        test <@ toPostFix(Sin(
                           Mul(
                            Add(Const(7.0), Const(14.0)), 
                            Sub(Const(6.0), Const(12.0))))) 
                 = "7.0 14.0 + 6.0 12.0 - * sin" 
             @>
