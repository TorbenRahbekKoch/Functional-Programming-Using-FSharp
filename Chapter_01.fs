module Chapter_01


// 1.1
let g n =
    n + 4

// 1.2
let h x y =
    System.Math.Sqrt(x*x + y*y)

// 1.3
// Note! One thing that confuses me here is the implicit parameter! I started writing:
// let gf n = function - which really gives some rather non-obvious results - (try it!).
// This is a fact that the book totally forgets to mention.
let gf = function
    | x -> (x + 4)


let hf = function
    | (x, y) -> System.Math.Sqrt(x*x + y*y)

// 1.4
// How to avoid the incomplete matches warning?
let rec f14 = function
    | 0 -> 0
    | 1 -> 1
    | n when n < 0 -> 0
    | n when n > 0 -> n + (n-1 |> f14)

// 1.5
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> (n-1 |> fib) + (n-2 |> fib)


// 1.6
let rec sum (m, n) =  
    match (m, n) with
    | (m, 0) -> m
    | (m, n) -> (m + n) + sum(m, n-1)

// 1.7
