module Cfa

// Label marks each element in syntax
type Label = int

type Variable = string

type PrimitiveOp =
    | Add
    | If
    | Stop
    | IsZero

type Simple =
    | Reference of binder : Label * label : Label * variable : Variable
    | Lambda of label : Lambda
    | Int of label : Label * value : int
    | Bool of label : Label * value : bool
    | PrimitiveOp of label : Label * op : PrimitiveOp

and Complex =
    | Call of label : Label * func : Simple * args : Simple list
    | Letrec of label : Label * bindings : (Variable * Simple) list * body : Complex

and Lambda = Label * Variable list * Complex

type Program = Lambda

// Contour or dynamic frame
type Frame = int

type Environment = Map<Label, Frame>

type Closure = Lambda * Environment

type Value =
    | Int of int
    | Bool of bool
    | Closure of Closure
    | PrimitiveOp of PrimitiveOp

type Store = Map<Variable * Frame, Value>

type Answer =
    | Value of Value
    | Error

let applyEnvironment label env =
    Map.find label env

let applyStore variable frame store =
    Map.find (variable, frame) store

let evaluate expr env store =
    match expr with
    | Simple.Int(_, n) -> Value.Int(n)
    | Simple.Bool(_, b) -> Value.Bool(b)
    | Simple.Reference(binder, label, variable) ->
        let frame = applyEnvironment binder env
        applyStore variable frame store
    | Simple.Lambda(lambda) -> Closure(lambda, env)
    | Simple.PrimitiveOp(label, op) -> PrimitiveOp(op)

let isFunction =
    function
    | PrimitiveOp(_) -> true
    | Closure(_) -> true
    | _ -> false

let toBool =
    function
    | Bool(false) -> false
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

let rec applyFunction funcValue argValues store =
    match funcValue with
    | PrimitiveOp(Stop) ->
        if List.length argValues = 1 then
            Value(argValues.[0])
        else
            Error
    | Int(_) | Bool(_) -> Error
    | Closure((label, args, body), env) ->
        if List.length args <> List.length argValues then
            Error
        else
            let frame = freshFrame()
            let env2 = Map.add label frame env
            let store2 = updateStore argValues args frame store
            prepareCall body env2 store2
    | PrimitiveOp(Add) ->
        match argValues with
        | [Int a; Int b; cont] -> applyFunction cont [Int(a + b)] store
        | _ -> Error
    | PrimitiveOp(If) ->
        match argValues with
        | [cond; thenK; elseK] when isFunction thenK && isFunction elseK ->
            if toBool cond then
                applyFunction thenK [] store
            else
                applyFunction elseK [] store
        | _ -> Error
    | PrimitiveOp(IsZero) ->
        match argValues with
        | [Int 0; cont] -> applyFunction cont [Bool(true)] store
        | [_; cont] -> applyFunction cont [Bool(false)] store
        | _ -> Error

and prepareCall complex env store =
    match complex with
    | Complex.Call(label, func, args) ->
        let funcVal = evaluate func env store
        let argValues = List.map (fun arg -> evaluate arg env store) args
        applyFunction funcVal argValues store
    | Complex.Letrec(label, bindings, body) ->
        let frame = freshFrame()
        let env2 = Map.add label frame env
        let folder store1 (name, expr) =
            let value = evaluate expr env2 store
            Map.add (name, frame) value store1
        let store2 = List.fold folder store bindings
        prepareCall body env2 store2

let runProgram programLambda =
    let env = Map.empty
    let closure = Closure(programLambda, env)
    let store = Map.empty
    let answ = applyFunction closure [PrimitiveOp(Stop)] store
    printfn "answer is %A" answ