module ExactCfa

// Exact control flow analysis for CPS

type Frame = int

type Environment = Map<Cps.Label, Frame>

type Closure = Cps.Lambda * Environment

type ValueInner =
    | Int of int
    | Bool of bool
    | Closure of Closure
    | PrimitiveOp of Cps.PrimitiveOp

type Value = Cps.Label * ValueInner

type Store = Map<Cps.Variable * Frame, Value>

type CallCacheEntry = Cps.Label * Environment * Value

type Answer = CallCacheEntry list

let callSiteAdd = -1
let callSiteIfTrue = -2
let callSiteIfFalse = -3
let callSiteIsZeroTrue = -4
let callSiteIsZeroFalse = -5

let applyEnvironment label env =
    Map.find label env

let applyStore variable frame (store : Store) =
    Map.find (variable, frame) store

let evaluate expr env (store : Store) =
    match expr with
    | Cps.Int(label, n) -> (label, Int(n))
    | Cps.Bool(label, b) -> (label, Bool(b))
    | Cps.Reference(binder, label, variable) ->
        let frame = applyEnvironment binder env
        applyStore variable frame store
    | Cps.Lambda((label, _, _) as lambda) -> (label, Closure(lambda, env))
    | Cps.PrimitiveOp(label, op) -> (label, PrimitiveOp(op))

let isFunction (_, v) =
    match v with
    | PrimitiveOp(_) -> true
    | Closure(_) -> true
    | _ -> false

let toBool =
    function
    | _, Bool(false) -> false
    | _ -> true

let freshFrame : unit -> Frame =
    let mutable count = 0
    fun () ->
        count <- count + 1
        count

let updateStore argValues argNames frame store : Store =
    Seq.zip argValues argNames
    |> Seq.fold (fun store (value, name) ->
        Map.add (name, frame) value store) store

let rec applyFunction (label, funcValue) argValues store =
    match funcValue with
    | PrimitiveOp(Cps.Stop) ->
        []
    | Int(_) | Bool(_) -> []
    | Closure((_, args, body), env) ->
        if List.length args <> List.length argValues then
            []
        else
            let frame = freshFrame()
            let env2 = Map.add label frame env
            let store2 = updateStore argValues args frame store
            prepareCall body env2 store2
    | PrimitiveOp(Cps.Add) ->
        match argValues with
        | [_, Int a; _, Int b; cont] ->
            let frame = freshFrame()
            let env2 = Map.ofList [label, frame]
            let entry = (callSiteAdd, env2, cont)
            entry :: applyFunction cont [label, Int(a + b)] store
        | _ -> []
    | PrimitiveOp(Cps.If) ->
        match argValues with
        | [cond; thenK; elseK] when isFunction thenK && isFunction elseK ->
            let frame = freshFrame()
            let env2 = Map.ofList [label, frame]
            let callSite, cont = if toBool cond then (callSiteIfTrue, thenK) else (callSiteIfFalse, elseK)
            let entry = (callSite, env2, cont)
            entry :: applyFunction cont [] store
        | _ -> []
    | PrimitiveOp(Cps.IsZero) ->
        let frame = freshFrame()
        let env2 = Map.ofList [label, frame]
        match argValues with
        | [_, Int 0; cont] ->
            (callSiteIsZeroTrue, env2, cont) :: applyFunction cont [label, Bool(true)] store
        | [_; cont] ->
            (callSiteIsZeroFalse, env2, cont) :: applyFunction cont [label, Bool(false)] store
        | _ -> []

and prepareCall complex env store =
    match complex with
    | Cps.Call(label, func, args) ->
        let funcVal = evaluate func env store
        let argValues = List.map (fun arg -> evaluate arg env store) args
        applyFunction funcVal argValues store
    | Cps.Letrec(label, bindings, body) ->
        let frame = freshFrame()
        let env2 = Map.add label frame env
        let folder store1 (name, expr) =
            let value = evaluate expr env2 store
            Map.add (name, frame) value store1
        let store2 = List.fold folder store bindings
        prepareCall body env2 store2

let runProgram programLambda =
    let label, _, _ = programLambda
    let env = Map.empty
    let closure = Closure(programLambda, env)
    let store = Map.empty
    let answ = applyFunction (label, closure) [0, PrimitiveOp(Cps.Stop)] store
    printfn "answer is %A" answ