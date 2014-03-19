module chapter_08_ex_2

// This is accepted by F# because the type inference is soooo cool.
// If you only execute the first or the two first lines it will not
// work, but when the third line with f(1) is executed it can figure
// out that x in the second line is of type int and therefore
// a must be a list of int.
let mutable a = []
let f x = a <- (x::a)
f(1)                  // 1 is of type int (per definition)
