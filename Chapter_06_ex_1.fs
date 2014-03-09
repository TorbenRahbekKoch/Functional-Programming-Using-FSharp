module Chapter_06_
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open NUnit.Framework
open Swensen.Unquote

// 6.1 expression reduction
type Fexpr = | Const of float
             | X
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr

let rec reduce expression =
    match expression with
    | Add(Const(left), Const(right))    -> Const(left + right)
    | Add(leftOp, Const(0.0))           -> reduce(leftOp)
    | Add(Const(0.0), rightOp)          -> reduce(rightOp)
    | Add(left, right)                  -> reduce(Add(reduce(left), reduce(right)))

    | Sub(Const(left), Const(right))    -> Const(left - right)
    | Sub(leftOp, Const(0.0))           -> reduce(leftOp)
    | Sub(left, right)                  -> reduce(Sub(reduce(left), reduce(right)))
  //| Sub(Const(0.0), rightOp)          -> reduce(rightOp)

    | Mul(Const(left), Const(right))    -> Const(left * right)
    | Mul(leftOp, Const(0.0))           -> Const(0.0)
    | Mul(Const(0.0), rightOp)          -> Const(0.0)
    | Mul(left, right)                  -> reduce(Mul(reduce(left), reduce(right)))

    | Div(Const(left), Const(right))    -> Const(left / right)
    //| Div(leftOp, Const(0.0))           -> Const(0.0)
    | Div(Const(0.0), rightOp)          -> Const(0.0)
    | Div(left, right)                  -> reduce(Div(reduce(left), reduce(right)))

    | Sin(expr)                         -> Sin(reduce(expr))
    | Cos(expr)                         -> Cos(reduce(expr))
    | Log(expr)                         -> Log(reduce(expr))
    | Exp(expr)                         -> Exp(reduce(expr))

    | expression                        -> expression

[<TestFixture>]
type ``Chapter 06 exercise 1 Tests``() = 
    [<Test>]
    member x.``6.1 reducing trivial expressions``() = 
        test <@ reduce(Add(Const(7.0), Const(14.0))) = Const(21.0)@>
        test <@ reduce(Add(Const(0.0), Const(14.0))) = Const(14.0)@>
        test <@ reduce(Add(Const(7.0), Const(0.0))) = Const(7.0)@>

        test <@ reduce(Sub(Const(7.0), Const(14.0))) = Const(-7.0)@>

        test <@ reduce(Mul(Const(7.0), Const(14.0))) = Const(98.0)@>
        test <@ reduce(Mul(Const(0.0), Const(14.0))) = Const(0.0)@>
        test <@ reduce(Mul(Const(7.0), Const(0.0))) = Const(0.0)@>

        test <@ reduce(Div(Const(7.0), Const(14.0))) = Const(0.5)@>
        test <@ reduce(Div(Const(0.0), Const(14.0))) = Const(0.0)@>

        test <@ reduce(Add(Mul(Const(7.0), Const(2.0)), Div(Const(10.0), Const(5.0)))) = Const(16.0)  @>
        test <@ reduce(Sub(Mul(Const(7.0), Const(2.0)), Div(Const(10.0), Const(5.0)))) = Const(12.0)  @>
        test <@ reduce(Mul(Mul(Const(7.0), Const(2.0)), Div(Const(10.0), Const(5.0)))) = Const(28.0)  @>
        test <@ reduce(Div(Mul(Const(7.0), Const(2.0)), Div(Const(10.0), Const(5.0)))) = Const(7.0)  @>

        test <@ reduce(Sin(Mul(Const(7.0), Const(2.0)))) = Sin(Const(14.0))@>
        test <@ reduce(Cos(Mul(Const(7.0), Const(2.0)))) = Cos(Const(14.0))@>
        test <@ reduce(Log(Mul(Const(7.0), Const(2.0)))) = Log(Const(14.0))@>
        test <@ reduce(Exp(Mul(Const(7.0), Const(2.0)))) = Exp(Const(14.0))@>
