module Chapter_04_ex_4

// 4.4 - This I cannot figure out...
let rec altsum = function
    | [] -> 0
    | x0::x1::xs -> x0-x1 + altsum xs