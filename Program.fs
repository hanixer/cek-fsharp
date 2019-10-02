// Learn more about F# at http://fsharp.org

open System
open Cfa

let program : Program =
    123, ["k"], Call(1, Simple.PrimitiveOp(4, Add), [Simple.Int(2, 1); Simple.Int(3, 2); Reference(123, 55, "x")])

[<EntryPoint>]
let main argv =
    let answer = runProgram program
    printfn "%A" answer
    0 // return an integer exit code
