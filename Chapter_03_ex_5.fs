module Chapter_03_ex_5

type Equation = float*float*float

type Solution =
    | NoSolution
    | OneRoot of float
    | TwoRoots of float*float

let solve (a, b, c)=
    let discriminant = b*b - 4.0*a*c
    if discriminant < 0.0 then
        NoSolution
    else
        let sqrtDiscriminant = sqrt(discriminant)
        let x1 = (-b + sqrtDiscriminant)/ 2.0 * a
        if a = 0.0 then
            OneRoot(x1)
        else
            let x2 = (-b - sqrtDiscriminant)/ 2.0 * a
            TwoRoots(x1, x2)
