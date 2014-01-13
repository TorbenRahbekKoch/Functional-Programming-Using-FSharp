module Chapter_03_ex_7

let isShape x = true

type Shape =
    | Circle of float
    | Square of float
    | Triangle of float*float*float
    
let area x =
    match x with
    | _ when not (isShape x) -> failwith "not a legal shape"
    | Circle r -> System.Math.PI * r * r
    | Square a -> a*a
    | Triangle(a, b, c) -> 42.0