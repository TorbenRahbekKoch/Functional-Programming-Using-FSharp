module Chapter_02
open System


// 2.1
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

// 2.4 - indices outside the range results in null
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