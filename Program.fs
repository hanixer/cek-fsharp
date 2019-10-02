// Learn more about F# at http://fsharp.org

open System

open Cesk

let p : Program = [
    "Main", "", [], [
        "main", ["a"; "b"; "c"], [
            MethodCall("a", Reference(thisName), "submain", [])
        ]
        "submain", [], [
            Return(Expression.Operation(Add, [Expression.Int(1); Expression.Int(2)]))
        ]
    ]
]

[<EntryPoint>]
let main argv =
    runProgram p


    0 // return an integer exit code
