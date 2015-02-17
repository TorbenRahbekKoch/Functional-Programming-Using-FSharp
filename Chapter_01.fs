module Chapter_01


// 1.1 - well, even to a beginner this is child's stuff ;)
let g n =
    n + 4

// 1.2
let h1 x y =
    System.Math.Sqrt(x*x + y*y)

// Why is it necessary to give a type here??
// Well, the reason is that sqrt is only defined for the types float and double. 
// Think of it as an extension method to those types. Since there are two types
// to choose from, F# cannot possibly figure out the right one. It therefore 
// falls back to the default inferred type: int - which means that it thinks 
// that both x and y are ints, and sqrt is not defined for int.  Why, then, 
// it is enough to add a return type (: float), I'm not at all sure of, though...
let h2 x y : float =
    sqrt(x*x + y*y)

// 1.3
// Note! One thing that confuses me here is the implicit parameter! I started 
// writing:
// let gf n = function - which really gives some rather non-obvious results 
// (see gfn below). The implicit parameter the book totally forgets to mention.
let gf = function
    | x -> x + 4

let gfn n = function
    | x-> x + 4

let hf1 = function
    | (x, y) -> System.Math.Sqrt(x*x + y*y)

// Again why necessary to give the type? Same reason as in 1.2 above.
let hf2 = function
    | (x:float, y:float) -> sqrt(x*x + y*y)

// 1.4
// How to avoid the incomplete matches warning? Well, F# does not take the 
// when clauses into consideration, when figuring out what matches and what
// does not.
// Please note that this implementation will do a stackoverflov on negative
// values and on large values, since it is not tail-recursive
let rec f14 = function
    | 0 -> 0
    | 1 -> 1
    | n when n < 0 -> 0
    | n when n > 0 -> n + (n-1 |> f14)
// f n = 10

// To avoid the incomplete matches warning it helps to have understood the 
// assignment correctly. The following implementations thanks to 
// Ramón Soto Mathiesen:

let rec f = function // stackoverflow issue (on negative and large values)
  | 1 -> 1
  | n -> n + f (n-1)

let f' n = // tail-recursive (chapter 9)
  let rec f'' a = function
    | 1 -> (1+a)
    | n -> f'' (a+n) (n-1)
  f'' 0 n

let f'' n = // cps - continuation passing style (chapter 9)
  let rec f''' k = function
    | 1 -> k 1
    | n -> f''' (fun x -> k (n+x)) (n-1)
  f''' id n

// 1.5 - The naive implementation, which has infinte recursion on negative and
// large values and of course is horribly inefficient, since it calculates the 
// values twice.
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> (n-1 |> fib) + (n-2 |> fib)
//    | fib (n-1) + fib (n-2) // alternative way of writing the recursive call 

// The following implementations by Ramón Soto Mathiesen:

// chapter 9
let fib' n =  // cps - continuation passing style (still stackoverflow issue)
  let rec fib'' k = function
    | 0 -> k 0
    | 1 -> k 1
    | n -> fib''(fun x -> fib''(fun y -> k(x+y)) (n-2)) (n-1)
  fib'' id n

let rec fib'' n = // tail-recursive with to accs (chapter 9)
  let rec fib''' a1 a2 = function
    | 0 -> 0
    | 1 -> a1 + a2
    | n -> fib''' a2 (a1 + a2) (n - 1)
  fib''' 1 0 n

type ContinuationBuilder() = // chapter 12
  member b.Bind(x, f) = fun k -> x (fun x -> f x k)
  member b.Return x = fun k -> k x
  member b.ReturnFrom x = x
let cont = ContinuationBuilder()
let fib''' n = // cps with monads (still stackoverflow issue)
  let rec fib'''' n = cont {
    match n with
    | 0 -> return 0
    | 1 -> return 1
    | n ->
      let! x = fib''''(n-1)
      let! y = fib''''(n-2)
      return x + y}
  fib'''' n id


// 1.6 - stackoverflow issues
let rec sum (m, n) =  
    match (m, n) with
    | (m, 0) -> m
    | (m, n) -> (m + n) + sum(m, n-1)

// The following implementation by Ramón Soto Mathiesen:
let sum' (m,n) = // tail-recursive
  let rec sum'' a = function
    | (m,0) -> (m+a)
    | (m,n) -> sum'' (a+m+n) (m,n-1)
  sum'' 0 (m,n)    


// 1.7 - both fact and power has obvious stackoverflow issues.
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)

let rec power = function
    | (x, 0) -> 1.0
    | (x, n) -> x* power(x, n-1)
      

let t1 = (System.Math.PI, fact -1) : float * int

let t2 = fact(fact 4): int

let t3 = power(System.Math.PI, fact 2) : float

let t4 = (power, fact) : (float * int -> float) * (int -> int)


// 1.8
let a = 5
// As a curiosum, try writing: let f18 a = a +1
// It gives a rather unexpected result.
let f18 a = a + 1

// (using g18 (underscore) because g is already used above)
let g18 b = (f b) + a

//       | a   |-> 5                      |  
// env = | f18 |-> "the add one function" |
//       | g18 |-> "the add six function" |

//    f18 3
// ~> 3 + 1
// ~> 4

//    g18 3
// ~> (f18 3) + a
// ~> (3 + 1) + a
// ~> 4 + 5
// ~> 9
