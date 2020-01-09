module KCfa

open Display

// k-CFA

/// Time is a list of previously called functions - call stack.
type Time = Cps.CallInfo list

type Environment = Map<Cps.Variable, Time>

type Closure = Cps.Lambda * Environment

type Value =
    | Closure of Closure
    | PrimitiveOp of Cps.PrimitiveOp

type Values = Set<Value>

type Bind = Cps.Variable * Time

type Store = Map<Bind, Values>

type State =
    | Eval of Cps.CallInfo * Environment * Store * Time
    | Apply of Value * List<Set<Value>> * Store * Time

let applyEnvironment label (env : Environment) =
    Map.find label env

let applyStore variable time (store : Store) =
    Map.find (variable, time) store

let nextTime state (time : Time) =
    match state with
    | Eval(call, _, _, _) -> [call]
    | _ -> time

let evaluate expr (env : Environment) (store : Store) =
    match expr with
    | Cps.Reference(binder, label, variable) ->
        let time = applyEnvironment variable env
        applyStore variable time store
    | Cps.Lambda((label, _, _) as lambda) -> Set.singleton(Closure(lambda, env))
    // | Cps.Int(label, n) -> (label, Int(n))
    // | Cps.Bool(label, b) -> (label, Bool(b))
    // | Cps.PrimitiveOp(label, op) -> (label, PrimitiveOp(op))

let addToStore store (bind, values) : Store =
    match Map.tryFind bind store with
    | Some(values2) -> Map.add bind (Set.union values values2) store
    | _ -> Map.add bind values store

let extendStore time argNames argValues store =
    let binders = List.map (fun argName -> (argName, time)) argNames
    Seq.fold addToStore store (Seq.zip binders argValues)

let step state =
    match state with
    | Eval((label, func, args), env, store, time : Time) ->
        let time2 = nextTime state time
        let functions = evaluate func env store
        let args = List.map (fun e -> evaluate e env store) args
        Set.map (fun func -> Apply(func, args, store, time2)) functions
    | Apply(Closure((label, argNames, Cps.Call(call)), env), argValues, store, time) ->
        let time2 = nextTime state time
        let env2 = List.fold (fun env arg -> Map.add arg time2 env) env argNames
        let store2 = extendStore time2 argNames argValues store
        Set.singleton(Eval(call, env2, store2, time2))
    | Apply(PrimitiveOp(Cps.Stop), _, _, _) -> Set.singleton(state)
    | Apply(_, _, _, _) -> failwith "Not Implemented"

let isFunction (_, v) =
    match v with
    | PrimitiveOp(_) -> true
    | Closure(_) -> true
    | _ -> false

let showValue value =
    match value with
    | Closure((_, args, _), _) ->
        let args = List.map iStr args |> iInterleave (iStr " ")
        iConcat [iStr ":(lambda "; args; iStr ")"]
    | PrimitiveOp(s) -> iStr (sprintf "%A" s)

let showEntry (d, value) =
    iConcat [iStr (sprintf "%A" d); iStr ":"; iNewline; showValue value; iNewline]

let showTime time =
    List.map (fun (label, _, _) -> iNum label) time
    |> iInterleave (iStr ", ")

let showValues values =
    iConcat [
        iStr "{"
        Seq.map showValue values |> Seq.toList |> iInterleave (iStr ", ");
        iStr "}"
    ]

let showStore (store : Store) =
    let x =
        Map.toList store
        |> List.map (fun ((variable, time), values) ->
            iConcat [iStr variable; iStr ": "; showTime time; iNewline; showValues values; iStr ";"; iNewline])
        |> iConcat
    iConcat [iStr "store:"; iNewline; iStr "  "; iIndent x]

let showState =
    function
    | Eval((label, _, _), _, store, time) ->
        iConcat [iStr "eval "; iStr "(call label "; iNum label; iStr ") "; iStr "(time "; showTime time; iStr ")"; iNewline; showStore store; ]
    | Apply(func, _, store, time) ->
        iConcat [iStr "apply "; iStr "(func "; showValue func; iStr ") "; iStr "(time "; showTime time; iStr ")"; iNewline; showStore store; ]

let entryToString entry = showEntry entry |> iDisplay

let rec explore visited todo =
    match todo with
    | state :: _ when Set.contains state visited -> visited
    | state :: rest ->
        let state = List.head todo
        printfn "-------------------------------------"
        printfn "%s" (iDisplay (showState state))
        let newStates = step state
        let visited2 = Set.add state visited
        let todo2 = List.append (Set.toList newStates) (List.tail todo)
        explore visited2 todo2
    | _ -> visited

let getStore =
    function
    | Eval(_, _, store, _) -> store
    | Apply(_, _, store, _) -> store

let mapVarToValues states =
    let addToStore store (bind, values) : Store =
        match Map.tryFind bind store with
        | Some(values2) -> Map.add bind (Set.union values values2) store
        | _ -> Map.add bind values store

    let handleStore map store =
        Map.fold (fun map (variable, _) values ->
            let values =
                match Map.tryFind variable map with
                | Some(values2) -> Set.union values values2
                | _ -> values
            Map.add variable values map) map store

    Seq.fold (fun map state ->
        let store = getStore state
        handleStore map store) Map.empty states

let runProgram programLambda =
    let env = Map.empty
    let closure = Closure(programLambda, env)
    let store = Map.empty
    let apply = Apply(closure, [Set.singleton(PrimitiveOp(Cps.Stop))], store, [])
    let states = explore Set.empty [apply]
    let varToValues = mapVarToValues states
    Map.iter (fun variable values ->
        printfn "%s:" variable
        let values = Seq.map showValue values |> Seq.toList
        printfn "%s:" (iDisplay (iInterleave iNewline values))) varToValues

