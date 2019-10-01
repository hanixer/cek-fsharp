// Learn more about F# at http://fsharp.org

open System
open Cek

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let term = c (l "x" (r "x")) (l "y" (r "y"))
    evaluate term
    0 // return an integer exit code
