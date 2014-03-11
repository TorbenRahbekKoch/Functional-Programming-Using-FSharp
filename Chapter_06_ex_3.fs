module Chapter_06_ex_3
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

// 6.3 - better toString
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

let rec toString = function
    | Const x       -> string x
    | X             -> "x"
    | Add(fe1,fe2)  -> "(" + (toString fe1) + ")"
                       + " + " + "(" + (toString fe2) + ")"
    | Sub(fe1,fe2)  -> "(" + (toString fe1) + ")"
                       + " - " + "(" + (toString fe2) + ")"
    | Mul(fe1,fe2)  -> "(" + (toString fe1) + ")"
                       + " * " + "(" + (toString fe2) + ")"
    | Div(fe1,fe2)  -> "(" + (toString fe1) + ")"
                       + " / " + "(" + (toString fe2) + ")"
    | Sin fe        -> "(sin " + (toString fe) + ")"
    | Cos fe        -> "(cos " + (toString fe) + ")"
    | Log fe        -> "(log " + (toString fe) + ")"
    | Exp fe        -> "(exp " + (toString fe) + ")";;

// It takes a LOT of patterns to complete this! (And a lot of tests to verify...)
// It would be possible to reduce the amount of patterns by using when expressions
// but I actually prefer patterns. I think I have all the permutations covered ;9
let rec betterToString = function
    | Const value                   -> String.Format(CultureInfo.InvariantCulture, "{0:0.0######}", value)
    | X                             -> "x"


    | Add((Mul(_,_) as mul1), (Mul(_,_) as mul2))   -> "(" + (betterToString mul1) + ")"
                                                       + " + " + "(" + (betterToString mul2) + ")"
    | Add((Div(_,_) as div1), (Div(_,_) as div2))   -> "(" + (betterToString div1) + ")"
                                                       + " + " + "(" + (betterToString div2) + ")"
    | Add((Mul(_,_) as mul), (Div(_,_) as div))     -> "(" + (betterToString mul) + ")"
                                                        + " + " + "(" + (betterToString div) + ")"
    | Add((Div(_,_) as div), (Mul(_,_) as mul))     -> "(" + (betterToString div) + ")"
                                                        + " + " + "(" + (betterToString mul) + ")"
    | Add((Mul(_,_) as mul), fe2)                   -> "(" + (betterToString mul) + ")"
                                                       + " + " + (betterToString fe2) 
    | Add((Div(_,_) as div), fe2)                   -> "(" + (betterToString div) + ")"
                                                       + " + " + (betterToString fe2) 
    | Add(fe1, (Mul(_,_) as mul))                   -> (betterToString fe1) 
                                                       + " + " + "(" + (betterToString mul) + ")"
    | Add(fe1, (Div(_,_) as div))                   -> (betterToString fe1) 
                                                       + " + " + "(" + (betterToString div) + ")"
    | Add(fe1,fe2)                                  -> (betterToString fe1) + " + " + (betterToString fe2)


    | Sub((Mul(_,_) as mul1), (Mul(_,_) as mul2))   -> "(" + (betterToString mul1) + ")"
                                                       + " - " + "(" + (betterToString mul2) + ")"
    | Sub((Div(_,_) as div1), (Div(_,_) as div2))   -> "(" + (betterToString div1) + ")"
                                                       + " - " + "(" + (betterToString div2) + ")"
    | Sub((Mul(_,_) as mul), (Div(_,_) as div))     -> "(" + (betterToString mul) + ")"
                                                       + " - " + "(" + (betterToString div) + ")"
    | Sub((Div(_,_) as div), (Mul(_,_) as mul))     -> "(" + (betterToString div) + ")"
                                                       + " - " + "(" + (betterToString mul) + ")"
    | Sub((Mul(_,_) as mul), fe2)                   -> "(" + (betterToString mul) + ")"
                                                       + " - " + (betterToString fe2) 
    | Sub((Div(_,_) as div), fe2)                   -> "(" + (betterToString div) + ")"
                                                       + " - " + (betterToString fe2) 
    | Sub(fe1, (Mul(_,_) as mul))                   -> (betterToString fe1) 
                                                       + " - " + "(" + (betterToString mul) + ")"
    | Sub(fe1, (Div(_,_) as div))                   -> (betterToString fe1) 
                                                       + " - " + "(" + (betterToString div) + ")"
    | Sub(fe1,fe2)                                  -> (betterToString fe1) + " - " + (betterToString fe2)


    | Mul((Add(_, _) as add1), (Add(_, _) as add2)) -> "(" + (betterToString add1) + ")"
                                                       + " * " + "(" + (betterToString add2) + ")"
    | Mul((Sub(_, _) as sub1), (Sub(_, _) as sub2)) -> "(" + (betterToString sub1) + ")"
                                                       + " * " + "(" + (betterToString sub2) + ")"
    | Mul((Add(_, _) as add), (Sub(_, _) as sub))   -> "(" + (betterToString add) + ")"
                                                       + " * " + "(" + (betterToString sub) + ")"
    | Mul((Sub(_, _) as sub), (Add(_, _) as add))   -> "(" + (betterToString sub) + ")"
                                                       + " * " + "(" + (betterToString add) + ")"
    | Mul(Add(_, _) as add, fe2)                    -> "(" + (betterToString add) + ")"
                                                       + " * " + (betterToString fe2)
    | Mul(Sub(_, _) as sub, fe2)                    -> "(" + (betterToString sub) + ")"
                                                       + " * " + (betterToString fe2)
    | Mul(fe1, (Add(_, _) as add))                  -> (betterToString fe1)
                                                       + " * " + "(" + (betterToString add) + ")"
    | Mul(fe1, (Sub(_, _) as sub))                  -> (betterToString fe1)
                                                       + " * " + "(" + (betterToString sub) + ")"
    | Mul(fe1,fe2)                                  -> "(" + (betterToString fe1) + ")"
                                                       + " * " + "(" + (betterToString fe2) + ")"

    | Div((Add(_, _) as add1), (Add(_, _) as add2)) -> "(" + (betterToString add1) + ")"
                                                       + " / " + "(" + (betterToString add2) + ")"
    | Div((Sub(_, _) as sub1), (Sub(_, _) as sub2)) -> "(" + (betterToString sub1) + ")"
                                                       + " / " + "(" + (betterToString sub2) + ")"
    | Div((Add(_, _) as add), (Sub(_, _) as sub))   -> "(" + (betterToString add) + ")"
                                                       + " / " + "(" + (betterToString sub) + ")"
    | Div((Sub(_, _) as sub), (Add(_, _) as add))   -> "(" + (betterToString sub) + ")"
                                                       + " / " + "(" + (betterToString add) + ")"
    | Div(Add(_, _) as add, fe2)                    -> "(" + (betterToString add) + ")"
                                                       + " / " + (betterToString fe2)
    | Div(Sub(_, _) as sub, fe2)                    -> "(" + (betterToString sub) + ")"
                                                       + " / " + (betterToString fe2)
    | Div(fe1, (Add(_, _) as add))                  -> (betterToString fe1)
                                                       + " / " + "(" + (betterToString add) + ")"
    | Div(fe1, (Sub(_, _) as sub))                  -> (betterToString fe1)
                                                       + " / " + "(" + (betterToString sub) + ")"
    | Div(fe1,fe2)                                  -> "(" + (betterToString fe1) + ")"
                                                       + " / " + "(" + (betterToString fe2) + ")"
    | Sin(Const(_) as c)                            -> "sin " + (betterToString c)
    | Sin(X as x)                                   -> "sin " + (betterToString x)
    | Sin fe                                        -> "sin" + "(" + (betterToString fe) + ")"

    | Cos(Const(_) as c)                            -> "cos " + (betterToString c)
    | Cos(X as x)                                   -> "cos " + (betterToString x)
    | Cos fe                                        -> "cos" + "(" + (betterToString fe) + ")"

    | Log(Const(_) as c)                            -> "log " + (betterToString c)
    | Log(X as x)                                   -> "log " + (betterToString x)
    | Log fe                                        -> "log" + "(" + (betterToString fe) + ")"

    | Exp(Const(_) as c)                            -> "exp " + (betterToString c)
    | Exp(X as x)                                   -> "exp " + (betterToString x)
    | Exp fe                                        -> "exp" + "(" + (betterToString fe) + ")"

[<TestFixture>]
type ``Chapter 06 exercise 3 Tests``() = 
    [<Test>]
    member x.``6.3 toString without uncessesary parenthesis``() = 
        test <@ betterToString (Mul(Add(Const(2.0), Const(3.0)), Sub(Const(4.0), Const(5.0)))) 
                                = "(2.0 + 3.0) * (4.0 - 5.0)" @>                        
        test <@ betterToString (Mul(Sub(Const(2.0), Const(3.0)), Add(Const(4.0), Const(5.0)))) 
                                = "(2.0 - 3.0) * (4.0 + 5.0)" @>

        test <@ betterToString (Mul(Sin(Const(3.0)), Sub(Const(4.0), Const(5.0)))) 
                                = "sin 3.0 * (4.0 - 5.0)" @>                        
        test <@ betterToString (Mul(Sub(Const(2.0), Const(3.0)), Sin(Const(4.0)))) 
                                = "(2.0 - 3.0) * sin 4.0" @>

        test <@ betterToString (Div(Add(Const(2.0), Const(3.0)), Sub(Const(4.0), Const(5.0)))) 
                                = "(2.0 + 3.0) / (4.0 - 5.0)" @>                        
        test <@ betterToString (Div(Sub(Const(2.0), Const(3.0)), Add(Const(4.0), Const(5.0)))) 
                                = "(2.0 - 3.0) / (4.0 + 5.0)" @>

        test <@ betterToString (Sin(Add(Const(2.0), Const(3.0)))) = "sin(2.0 + 3.0)" @>
