// Learn more about F# at http://fsharp.org

open System
open Cfa
open SExpr

let program : Program =
    123, ["k"], Call(1, Simple.PrimitiveOp(4, Add), [Simple.Int(2, 1); Simple.Int(3, 2); Reference(123, 55, "k")])

// 1 => (lambda (k) (k 1))
// (+ 1 2) => (lambda (k) (+ 1 2 k))
// (+ (+ 1 2) (+ 3 4)) => (+ 1 2 (lambda (a) (+ 3 4 (lambda (b) (+ a b)))))

let toCps sexpr =
    let mutable counter = 0
    let freshLabel() =
        counter <- counter + 1
        counter

    let bindingToName = function
        | List [Symbol name; _] -> name
        | _ -> failwith "wrong binding"

    let extendEnv names label env =
        List.fold (fun env name -> Map.add name label env) env names

    let rec parseComplex env sexpr =
        match sexpr with
        | List [Symbol "letrec"; List bindings; body] ->
            let label = freshLabel()
            let names = List.map bindingToName bindings
            let env2 = extendEnv names label env
            let bindings = parseBindings env2 bindings
            let body = parseComplex env2 body
            Letrec(label, bindings, body)
        | List(func :: args) ->
            let funcE = parseSimple env func
            let argsE = parseSimpleMany env args
            Call(freshLabel(), funcE, argsE)
        | _ -> failwith "wrong complex expression"

    and parseBindings env bindings =
        List.map (fun binding ->
            match binding with
            | List [Symbol name; rhs] -> (name, parseSimple env rhs)
            | _ -> failwith "wrong binding 2") bindings

    and parseSimple env sexpr =
        match sexpr with
        | Symbol "+" -> Simple.PrimitiveOp(freshLabel(), Add)
        | Symbol "if" -> Simple.PrimitiveOp(freshLabel(), If)
        | Symbol "zero?" -> Simple.PrimitiveOp(freshLabel(), IsZero)
        | Symbol variable ->
            let binder = Map.find variable env
            let label = freshLabel()
            Reference(binder, label, variable)
        | SExpr.Number(n) -> Simple.Int(freshLabel(), n)
        | SExpr.Bool(b) -> Simple.Bool(freshLabel(), b)
        | List [Symbol "lambda"; List args; body] ->
            let label = freshLabel()
            let args = symbolsToStrings args
            let env2 = extendEnv args label env
            let body = parseComplex env2 body
            Lambda(label, args, body)
        | _ -> failwith "wrong simple expr"

    and parseSimpleMany env sexprs = List.map (parseSimple env) sexprs

    match parseSimple Map.empty sexpr with
    | Lambda(lambda) -> lambda
    | _ -> failwith "lambda expected on top level"

// (lambda (stop)
//   (lambda (x y k) (+ x y k)) 1 2 stop)
// (lambda (stop) (letrec ((f (lambda (n k) (zero? n (lambda (z) (if z (lambda () 0) (lambda () (+ n -1) (lambda (m) (f m (lambda (fm) (+ n fm (lambda (plu) (k plu))))))))))))) (f 5 stop))
let s1 = "(lambda (stop) ((lambda (x y k) (+ x y k)) 1 2 stop))"
let s2 = "(lambda (stop) (letrec ((f (lambda (n k) (zero? n (lambda (z) (if z (lambda () (k 0)) (lambda () (+ n -1 (lambda (m) (f m (lambda (fm) (+ n fm k)))))))))))) (f 5 stop)))"

[<EntryPoint>]
let main argv =

    let program = (toCps (stringToSExpr s2))
    let answer = runProgram program
    printfn "%A" answer

    0 // return an integer exit code
