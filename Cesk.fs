module Cesk

// Language

type Operation =
    | Add

type Expression =
    | This
    | True
    | False
    | Null
    | Void
    | Reference of string
    | Int of int
    | Operation of Operation * Expression list
    | Instanceof of Expression * className : string
    | FieldAccess of Expression * fieldName : string

type Statement =
    | Label of string
    | Skip
    | Goto of label : string
    | If of condition : Expression * gotoLabel : string
    | New of lhs : string * className : string
    | MethodCall of lhs : string * Expression * methodName : string * Expression list
    | SuperCall of lhs : string * methodName : string * Expression list
    | AssignmentField of object : Expression * fieldName : string * Expression
    | Return of Expression
    | PushHandler of className : string * label : string
    | PopHandler
    | Throw of Expression
    | MoveException of name : string

type Body = Statement list

type MethodDefinition = string * string list * Body

type FieldDefinition = string

type ClassDefinition = string * string * FieldDefinition list * MethodDefinition list

type Program = ClassDefinition list

// State

type Pointer = int

type Offset = string

type Address = Pointer * Offset

type Object = string * Pointer

type Value =
    | Object of className : string * Pointer
    | Int of int
    | Void
    | Null
    | True
    | False

type Store = {
    Map : Map<Address, Value>
    Counter : int
}

type FramePointer = Pointer

type Continuation =
    | Assign of name : string * Statement list * FramePointer * Continuation
    | Handle of className : string * label : string * Continuation
    | Halt

type State = {
    Control : Statement list
    Environment : FramePointer
    Store : Store
    Kont : Continuation
    LabelsMap : Map<string, Statement list>
}

type MethodsMap = Map<string * string, MethodDefinition>

type ParentMap = Map<string, string>

let thisName = "this"

let freshPointer =
    let mutable count = 0
    fun () ->
        count <- count + 1
        count

let getClass program className =
    List.find (fun (name, _, _, _) -> name = className) program

let makeParentMap program =
    let classDef (name, parent, _, _) =
        let parentDef = List.find (fun (name, _, _, _) -> name = parent) program
        (name, parent)
    List.map classDef program
    |> Map.ofList


let resolveLabel label state =
    match Map.tryFind label state.LabelsMap with
    | Some(x) -> x
    | _ -> failwithf "Label '%s' not found" label

let getFromStore addr store =
    match Map.tryFind addr store.Map with
    | Some(x) -> x
    | _ -> failwithf "Address '%A' is invalid" addr

let updateStore addr value store = {store with Map = Map.add addr value store.Map}

let getAddress pointer offset : Address = (pointer, offset)

let lookupMethod program className methodName =
    let rec loop className =
        let (_, parent, _, methods) = getClass program className
        match List.tryFind (fun (name, _, _) -> name = methodName) methods with
        | Some(method) -> method
        | None -> loop parent

    loop className


let rec evaluate expression framePointer store =
    match expression with
    | Expression.True -> Value.True
    | Expression.False -> Value.False
    | Expression.Null -> Value.Null
    | Expression.Void -> Value.Void
    | Expression.This -> getFromStore (getAddress framePointer thisName) store
    | Expression.Reference(variable) -> getFromStore (getAddress framePointer variable) store
    | Expression.Int(n) -> Value.Int(n)
    | Expression.Operation(op, args) ->
        List.fold (fun acc arg ->
            match evaluate arg framePointer store with
            | Value.Int(n) -> acc + n
            | v -> failwithf "Expected Int, but got %A" v) 0 args
        |> Value.Int
    | Expression.Instanceof(object, className) ->
        match evaluate object framePointer store with
        | Object(name, _) when name = className -> Value.True
        | _ -> Value.False
    | Expression.FieldAccess(object, fieldName) ->
        match evaluate object framePointer store with
        | Object(_, pointer) -> getFromStore (getAddress pointer fieldName) store
        | v -> failwithf "Object expected, but got %A" v

let updateStoreArguments argNames argExprs framePointerNew storeNew framePointer store =
    let zipped = List.zip argNames argExprs
    List.fold (fun storeAcc (name, expr) ->
        let value = evaluate expr framePointer store
        updateStore (getAddress framePointerNew name) value storeAcc) storeNew zipped

let applyMethod method lhs nextStmts thisValue argExprs state =
    let (_, argNames, body) = method
    let framePointer1 = freshPointer()
    let store1 = updateStore (getAddress framePointer1 thisName) thisValue state.Store
    let store2 = updateStoreArguments argNames argExprs framePointer1 store1 state.Environment state.Store
    let cont1 = Assign(lhs, nextStmts, state.Environment, state.Kont)
    {state with Control = body; Environment = framePointer1; Store = store2; Kont = cont1}

let toBoolean =
    function
    | Value.False -> false
    | _ -> true

let getClassOfObject =
    function
    | Object(className, _) -> className
    | _ -> failwith "Wrong value, expected Object"

let getPointerOfObject =
    function
    | Object(_, pointer) -> pointer
    | _ -> failwith "Wrong value, expected Object"

let popHandler =
    function
    | Handle(_, _, cont) -> cont
    | k -> failwithf "Handle continuation is expected, but got %A" k

let applyContinuation cont value state =
    match cont with
    | Continuation.Assign(name, statements, framePointer, cont) ->
        let store = updateStore (getAddress framePointer name) value state.Store
        {state with Control = statements; Store = store; Kont = cont}
    | Continuation.Handle(className, label, _) -> failwith "Not Implemented"
    | Continuation.Halt -> failwith "Not Implemented"

let step program state =
    let statement = List.head state.Control
    let nextStmts = List.tail state.Control

    match statement with
    | Skip | Label(_) -> {state with Control = nextStmts}
    | Goto(label) ->
        let statements = resolveLabel label state
        {state with Control = statements}
    | If(condition, gotoLabel) ->
        let value = evaluate condition state.Environment state.Store
        if toBoolean value then
            let statements = resolveLabel gotoLabel state
            {state with Control = statements}
        else
            {state with Control = nextStmts}
    | New(lhs, className) ->
        let c = freshPointer()
        let object = Object(className, c)
        let address = (state.Environment, lhs)
        let store2 = {Map = Map.add address object state.Store.Map; Counter = c}
        let statements = nextStmts
        {state with Control = statements; Store = store2}
    | MethodCall(lhs, objectExpr, methodName, argExprs) ->
        let thisValue = evaluate objectExpr state.Environment state.Store
        let className = getClassOfObject thisValue
        let method = lookupMethod program className methodName
        applyMethod method lhs nextStmts thisValue argExprs state
    | SuperCall(lhs, methodName, argExprs) ->
        let thisValue = getFromStore (getAddress state.Environment thisName) state.Store
        let className = getClassOfObject thisValue
        let (_, parent, _, _) = getClass program className
        let method = lookupMethod program parent methodName
        applyMethod method lhs nextStmts thisValue argExprs state
    | AssignmentField(object, fieldName, expression) ->
        let lhs = evaluate object state.Environment state.Store
        let value = evaluate expression state.Environment state.Store
        let pointer = getPointerOfObject lhs
        let store1 = updateStore (getAddress pointer fieldName) value state.Store
        {state with Store = store1}
    | Return(expression) ->
        let value = evaluate expression state.Environment state.Store
        applyContinuation state.Kont value state
    | PushHandler(className, label) ->
        let cont = Continuation.Handle(className, label, state.Kont)
        {state with Kont = cont}
    | PopHandler -> {state with Kont = popHandler state.Kont}
    | Throw(_) -> failwith "Not Implemented"
    | MoveException(name) -> failwith "Not Implemented"
