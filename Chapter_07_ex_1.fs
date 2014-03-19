module Vector
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    
open System
open System.Globalization
open NUnit.Framework
open Swensen.Unquote

// 7.2

type Vector = {
    x: float; y: float
}

let make (x, y) =
    {x = x; y = y}

let coord vector = 
    (vector.x, vector.y)

let norm vector =
    sqrt(vector.x * vector.x + vector.y * vector.y)

type Vector with
static member (~-) vector =
    { x = -vector.x; y = -vector.y }

static member (+) (left, right) =
    { x = left.x + right.x; y = left.y + right.y }

static member (-) (left, right) =
    { x = left.x - right.x; y = left.y - right.y }

static member (*) (constant: float, vector) =
    { x = constant * vector.x; y = constant * vector.y }

static member (*) (left, right) =
    left.x * right.x + right.y * right.y

