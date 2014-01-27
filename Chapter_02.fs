module Chapter_02
open System

// 2.1 - again: remember the implicit parameter
let f = function
  | n when n % 2 = 0 -> true
  | n when n % 3 = 0 -> true
  | n when n % 5 = 0 -> false // Not strictly necessary but makes it explicit that 5 should result in false
  | _ -> false           

// 2.2 - here a recursive version
// String.Concat<T>(IEnumerable<T> values) could also be used
let rec pow s n = 
    match n with
    | x when x <= 0 -> "" // I choose to define it like this
    | 1 -> s
    | x -> s + pow s (n-1)
    
// 2.2 - a version using StringBuilder - preliminary timings suggest that this version is by far the most performant
let powWithBuilder (s: string) n =
    let builder = System.Text.StringBuilder()
    for count = 1 to n do
        builder.Append(s) |> ignore
    builder.ToString()

// 2.2 - a version using String.Concat - seems to be slowest
let powWithStringConcat s n =
    String.Concat(seq { for i in 1 .. n do yield s })

// 2.3 - checking for an index outside the range could either return false (as here) or throw an
// ArgumentOutOfRangeException.
// Note here, that it is necessary to explicitly define the type for s
let isIthChar ((s:string),  index,  ch) =
    if (index < 0 || index >= s.Length) then
        false; // throw ArgumentException("Should be inside Length of s", "index")
    else
        s.[index] = ch

// 2.4 - indices outside the range results in 0
// Using s.Substring for obtaining the part of the string from index and onwards.
let occFromIth(s: string, index, ch) = 
    if (index < 0 || index >= s.Length) then
        0
    else
        s.Substring index 
        |> Seq.filter(fun charAtIndex -> charAtIndex = ch)
        |> Seq.length

// 2.5 - Reusing our occFromIth function from above.
let occInString(s: string, ch) =
    occFromIth(s, 0, ch)

// 2.6 - If d < n then d will never be divisible by n
let notDivisible (d, n) =
    if (abs(d) < abs(n)) then
        true
    else
        not (d % n = 0)

// 2.7 1. - with pattern matching
let rec test(a, b, c) =
    match (a, b) with
    |_ when a > b -> failwith(String.Format("a should <= b: {0} <= {1}", a, b))
    |_ when a < b -> notDivisible(a, c) && test(a + 1, b, c)
    |_ -> notDivisible(a, c) 

// or using ifs - far less readable
let rec test_if(a, b, c) =
    if a > b then
        failwith(String.Format("a should <= b: {0} <= {1}", a, b))
    else
        if a < b then
            notDivisible(a, c) && test(a + 1, b, c)
        else
            notDivisible(a, c) 

// 2.7 2. 
// n is a prime if it cannot be divided by any integer from 2 to sqrt(n)
// I think the book intends us to use the test(a,b,c) function above, but it is
// defined incorrectly - a correctly defined should look like this (a and c is reversed):
let rec test_prime(a, b, c) =
    match (a, b) with
    |_ when a > b -> failwith(String.Format("a should <= b: {0} <= {1}", a, b))
    |_ when a < b -> notDivisible(c, a) && test_prime(a + 1, b, c)
    |_ -> notDivisible(c, a) 


// The matches with 1, 2, and 3 are to avoid b < a when using sqrt.
// Can the "(float)n |> sqrt |> Math.Floor |> (int)" be written more intelligible?
let prime n =
    match n with
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | _ when n < 1 -> false
    | _ -> test_prime(2, (float)n |> sqrt |> Math.Floor |> (int) , n)

// 2.7 3.
let rec nextPrime n =
    let isPrime = prime (n + 1)
    if isPrime then
        n + 1
    else
        nextPrime (n + 1)

// 2.8 Binomial coefficients
// This version is not fully tail-recursive, which might pose a problem for bigger n's and k's
// Furthermore it will calculate a lot of the numbers twice (I think...)
let rec bin(n, k) =
    match (n, k) with
    | (row, 0) -> 1
    | (row, col) when col = n -> 1
    | (row, col) -> bin(n - 1, k - 1) + bin(n - 1, k)

// 2.9
let rec f_ = function
    | (0, y) -> y
    | (x, y) -> f_(x-1, x*y)

// 1. The inferred type is int*int -> int
// 2. The function terminates for values (x, y) where x >= 0
// 3. 
//    f_(2, 3)
// ~> f_(2-1, 2*3)
// ~> f_(1, 6)
// ~> f_(1-1, 1*6)
// ~> f_(0, 6)
// ~> 6

//    f_(3, 2)
// ~> f_(3-1, 3*2)
// ~> f_(2, 6)
// ~> f_(2-1, 2*6)
// ~> f_(1, 12)
// ~> f_(1-1, 1*12)
// ~> f_(0, 12)
// ~> 12

//    f_(6, 6)
// ~> f_(6-1, 6*6)
// ~> f_(5, 36)
// ~> f_(5-1, 5*36)
// ~> f_(4, 180)
// ~> f_(4-1, 4*180)
// ~> f_(3, 720)
// ~> f_(3-1, 3*720)
// ~> f_(2, 2160)
// ~> f_(2-1, 2*2160)
// ~> f_(1, 4320)
// ~> f_(1-1, 1*4320)
// ~> f_(0, 4320)
// ~> 4320

// 4. f_(x, y) = ???


// 2.10
let test_(c, e) = if c then e else 0;;
// 1. The inferred type is bool*int -> int
// 2. This seems to result in a StackOverflowException (if the fact function is
// naively implemented)
// It does this, because fact(-1) is evaluated before calling test_
open Chapter_01 // access the fact function
let ``2.10 2. result`` = test_(false, fact(-1))

// 3. This, on the other hand, succeeds:
// It does this, because fact -1 is only evaluated/executed in the true branch of the if, which
// of course is never hit here.
let ``2.10 3. result`` = if false then fact -1 else 0

// 2.11 VAT / unVAT
let VAT n x = x + x * float n/100.0

let unVAT n x = x / ((100.0 + float n) / 100.0)

// 2.12 min(f n) 
// This is obviously a horribly ineffecient way to do this - but it cannot be done any
// different, can it???
let min f =     
    seq { for n in Int32.MinValue .. Int32.MaxValue do yield n}
    |> Seq.find (fun n -> f n = 0)
        
// 2.13 curry/uncurry
// I have no clue as to what this exercise wants me to do...
//let curry (f: 'a*'b -> 'c): 'a -> 'b -> 'c =
// ?????    
// Thanks to Rune Ibsen for this:
let curry f   = fun a -> fun b -> f (a,b)
let uncurry f = fun (a,b) -> f a b