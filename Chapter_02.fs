module Chapter_02
open System


// 2.1
let f = function
  | n when n % 2 = 0 -> true
  | n when n % 3 = 0 -> true
  | n when n % 5 = 0 -> false // Not strictly necessary but makes it explicit that 5 should result in false
  | _ -> false           

// 2.2 - a non-recursive version using a StringBuilder would likely be more effecient
// String.Concat<T>(IEnumerable<T> values) could also be used
let rec pow s n = 
    match n with
    | x when x <= 0 -> "" // I choose to define it like this
    | 1 -> s
    | x -> s + pow s (n-1)
    
let powWithBuilder (s: string) n =
    let builder = System.Text.StringBuilder()
    for count = 1 to n do
        builder.Append(s) |> ignore
    builder.ToString()

let powWithStringConcat s n =
    String.Concat(seq { for i in 1 .. n do yield s })