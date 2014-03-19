// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 7: Modules

// From Section 7.3 and 7.4: Signature file for vector mudule with type augmentation

module Vector
[<Sealed>]
type Vector =
    static member ( ~- ) : Vector -> Vector
    static member ( + )  : Vector * Vector -> Vector
    static member ( - )  : Vector * Vector -> Vector
    static member ( * )  : float  * Vector -> Vector
    static member ( * )  : Vector * Vector -> float
val make : float * float -> Vector
val coord: Vector -> float * float
val norm : Vector -> float
