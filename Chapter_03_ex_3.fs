module Chapter_03_ex_3


type Complex = float * float

// 3.3 1. Add and Multiply complex numbers represented as tuples
let addComplex (n1:Complex) (n2:Complex) =
    let a, b = n1
    let c, d = n2
    (a + c, b + d)

let mulComplex (n1:Complex) (n2:Complex) =
    let a, b = n1
    let c, d = n2
    (a*c - b*d, b*c + a*d)

let (.+) n1 n2 = addComplex n1 n2
let (.*) n1 n2 = mulComplex n1 n2

// 3.3 2. Subtraction and Division 
let subtractComplex (n1:Complex) (n2:Complex) =
    let a, b = n1
    let c, d = n2
    (a - c, b - d)

let divComplex (n1:Complex) (n2:Complex) =
    let a, b = n1
    let c, d = n2
    let divisor = (c*c + d*d)
    ((a*c + b*d)/divisor, ((b*c - a*d)/divisor))

let (.-)  n1 n2 = subtractComplex n1 n2
let (./)  n1 n2 = divComplex n1 n2

// 3.3 3.
// I usually do this without being told ;)