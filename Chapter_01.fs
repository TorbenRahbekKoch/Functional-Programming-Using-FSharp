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
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)

let rec power = function
    | (x, 0) -> 1.0
    | (x, n) -> x* power(x, n-1)
      
let t1 = (System.Math.PI, (fact) -1) : float * int

let t2 = fact(fact 4): int

let t3 = power(System.Math.PI, fact 2) : float

// why is the type not: (float * int -> float, int -> int) - aka a tuple?
let t4 = (power, fact) : (float * int -> float) * (int -> int)


// 1.8
let a = 5
// As a curiosum, try writing: let f a = a +1
// It gives a rather unexpected result.
let f a = a + 1

// Using g_ (underscore) because is already used above
let g_ b = (f b) + a

//       | a  |-> 5                      |  
// env = | f  |-> "the add one function" |
//       | g_ |-> "the add six function" |

//    f 3
// ~> 3 + 1
// ~> 4

//    g_ 3
// ~> (f 3) + a
// ~> (3 + 1) + a
// ~> 4 + 5
// ~> 9