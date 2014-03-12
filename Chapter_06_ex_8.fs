module Chapter_06_ex_8
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

// 6.8 1. Stack and Interpret Instruction
type Instruction = 
    | ADD | SUB | MUL | DIV | SIN | COS | LOG | EXP | PUSH of float

type Stack = float list

let interpretInstruction stack instruction = 
    match instruction, stack with
    | ADD, right::left::rest    -> (left + right)::rest
    | SUB, right::left::rest    -> (left - right)::rest
    | MUL, right::left::rest    -> (left * right)::rest
    | DIV, right::left::rest    -> (left / right)::rest
    | SIN, operand::rest        -> (sin operand)::rest
    | COS, operand::rest        -> (cos operand)::rest
    | LOG, operand::rest        -> (log operand)::rest
    | EXP, operand::rest        -> (exp operand)::rest
    | PUSH(value), stack        -> value::stack
    | _ -> failwith "Illegal stack state for operation"


// 6.8 2. interpretProgram
let interpretProgram instructions =
    let rec executeProgram (stack: float list) instructions =
        match instructions with
        | []                -> stack.Head
        | instruction::rest -> let resultStack = interpretInstruction stack instruction
                               executeProgram resultStack rest
    executeProgram [] instructions

// 6.8 3. transform expression tree

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

let rec transformFexpr expression x =
    match expression with
    | Const(value)      -> [PUSH(value)]
    | Add(left, right)  -> (transformFexpr left x) @ (transformFexpr right x) @ [ADD]
    | Sub(left, right)  -> (transformFexpr left x) @ (transformFexpr right x) @ [SUB]
    | Mul(left, right)  -> (transformFexpr left x) @ (transformFexpr right x) @ [MUL]
    | Div(left, right)  -> (transformFexpr left x) @ (transformFexpr right x) @ [DIV]
    | Sin(expr)         -> (transformFexpr expr x) @ [SIN]
    | Cos(expr)         -> (transformFexpr expr x) @ [COS]
    | Log(expr)         -> (transformFexpr expr x) @ [LOG]
    | Exp(expr)         -> (transformFexpr expr x) @ [EXP]
    | X                 -> []   // Not quite sure what to do with the X


[<TestFixture>]
type ``Chapter 06 exercise 8 Tests``() = 
    [<Test>]
    member x.``6.8 1. type Stack and interpretInstruction``() =    
        test <@ interpretInstruction [1.0;2.0] ADD = [3.0] @>
        test <@ interpretInstruction [1.0;2.0] SUB = [1.0] @>
        test <@ interpretInstruction [1.0;2.0] MUL = [2.0] @>
        test <@ interpretInstruction [1.0;2.0] DIV = [2.0] @>
        test <@ interpretInstruction [1.0] SIN = [sin 1.0]@>
        test <@ interpretInstruction [] (PUSH(42.0)) = [42.0]@>

    [<Test>]
    member x.``6.8 2. interpretProgram``() =    
        test<@ interpretProgram [PUSH(3.0);PUSH(4.0);ADD] = 7.0 @>
        test<@ interpretProgram [PUSH(3.0);PUSH(4.0);SUB] = -1.0 @>

    [<Test>]
    member x.``6.8 3. transformExpression``() =    
        test <@ (transformFexpr (Add(Const(7.0), Const(8.0))) X) = [PUSH(7.0);PUSH(8.0);ADD] @>
        test <@ 
                (transformFexpr (Add(Mul(Const(7.0), Const(8.0)), Div(Const(9.0), Const(10.0)))) X) 
                                = [PUSH(7.0);PUSH(8.0);MUL;PUSH(9.0);PUSH(10.0);DIV;ADD] 
             @>
