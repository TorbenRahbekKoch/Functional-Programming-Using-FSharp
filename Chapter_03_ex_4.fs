module Chapter_03_ex_4

// 3.4 1. 
// A record type for a straight line:
type StraightLineRec = {
    a: float
    b: float
}

// we can also use a tuple
type StraightLine = float*float

// 3.4 2. 
let mirrorX line =
    let a, b = line
    (-a, -b)

let mirrorXRec (line: StraightLineRec) =
    let a, b = line.a, line.b
    { a= -a; b= -b}

let mirrorY line =
    let a, b = line
    (-a, b)

let mirrorYRec (line: StraightLineRec) =
    let a, b = line.a, line.b
    { a= -a; b=b}

// 3.4 3.
let formatStraightLine line =
    let a, b = line
    sprintf "y = %fx + %f" a b

let formatStraightLineRec (line: StraightLineRec) =
    let a, b = line.a, line.b
    sprintf "y = %fx + %f" a b
