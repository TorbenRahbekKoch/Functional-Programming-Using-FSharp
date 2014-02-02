module Chapter_04_ex_9
open NUnit.Framework
open FsUnit

// 4.9 - naive recursive version
let rec zip (list1, list2) =
    match list1, list2 with
    | ([],[]) -> []
    | (head1::[], head2::[]) -> [(head1, head2)]
    | (head1::tail1, head2::tail2) when tail1.Length = tail2.Length -> 
        (head1, head2)::zip (tail1, tail2)
    | _ -> failwith "Different lengths"

[<TestFixture>]
type ``Chapter_04_ex_9_Tests``() = 
    [<Test>]
    member x.``4.9 recursive``() = 
        (fun () -> zip ([1;3], [2]) |> ignore) |> should throw typeof<System.Exception>
        zip ([1;3], [2;4]) |> should equal [(1,2);(3,4)]
