module rec Anf

/// K-CFA analysis for ANF language.
/// Uses allocation scheme from paper "Pushdown Control-Flow Analysis for Free" by Gilray

/// let x = e1 e2 in e3
/// x
/// lambda x: e1
/// number

type TermId = int

type Term =
    | LetCall of name: string * func: Term * arg: Term * body: Term * id: TermId
    | Let of name: string * value: Term * body: Term * id: TermId
    | Lambda of func: Lambda * id: TermId
    | Variable of name: string * id: TermId
    | Int of int * id: TermId

// Respresents argument and body of a function.
type Lambda = string * Term

/// Value address in store
type ValAddress = string * TermId

// Maps from variable name to values. Finite.
type Environment = Map<string, ValAddress>

/// Value store. Maps from value address to set of values
type Store = Map<ValAddress, Set<Value>>

// The only value for now is closure.
type Value =
    | Closure of Lambda * Environment
    | Int of int

/// Address of continuation in continuation store.
type ContAddress = TermId * Environment

/// Represents a call frame - to which variable the result must be bound,
/// which expression should be evaulated next in which environment,
/// and address of the next continuation.
type Frame = string * Term * Environment * ContAddress

/// Continuation store.
/// Each continuation address can correspond to multiple return points.
type ContStore = Map<ContAddress, Set<Frame>>

type State =
    { Term: Term
      Env: Environment
      Store: Store
      ContStore: ContStore
      ContAddress: ContAddress }

let contAddressHalt = (-1, Map.empty)

let getTermId term =
    match term with
    | Term.Int(id = id)
    | Variable(id = id)
    | Lambda(id = id)
    | LetCall(id = id) -> id

let addValueToStore addr value store =
    match Map.tryFind addr store with
    | Some(values) -> Map.add addr (Set.add value values) store
    | _ -> Map.add addr (Set.singleton value) store

let addManyToStore addr values store =
    match Map.tryFind addr store with
    | Some(values2) -> Map.add addr (Set.union values values2) store
    | _ -> Map.add addr values store

let returnValue termId value (state: State) =
    if state.ContAddress = contAddressHalt then
        Seq.empty
    else
        Seq.map (fun (frame: Frame) ->
            let (retName, retTerm, retEnv, nextContAddr) = frame
            let addr = (retName, termId)
            let env = Map.add retName addr retEnv
            let store = addValueToStore addr value state.Store
            { Term = retTerm
              Env = env
              Store = store
              ContStore = state.ContStore
              ContAddress = nextContAddr }) state.ContStore.[state.ContAddress]

let variableValue name state =
    let addr = state.Env.[name]
    state.Store.[addr]

let unwrapLambda value =
    match value with
    | Closure((arg, body), env) -> arg, body, env
    | _ -> failwith "closure is expected"

let performCall name body termId funcVals argVals state =
    seq {
        for func in funcVals do
            let arg, funcBody, funcEnv = unwrapLambda func
            let addr = (arg, termId)
            let env = Map.add arg addr funcEnv
            let store = Map.add addr argVals state.Store
            let contAddr = (getTermId funcBody, env)
            let frame = (name, body, state.Env, state.ContAddress)
            let contStore = addValueToStore contAddr frame state.ContStore
            { Term = funcBody
              Env = env
              Store = store
              ContStore = contStore
              ContAddress = contAddr }
    }

let evaluateSimple term state =
    match term with
    | Variable(name, id) -> variableValue name state
    | Term.Int(n, id) -> Set.singleton (Int(n))
    | Lambda(func, id) -> Set.singleton (Closure(func, state.Env))
    | _ -> failwith "simple expression expected"

let step state =
    match state.Term with
    | Variable(name, id) ->
        let values = variableValue name state
        Seq.collect (fun value -> returnValue id value state) values
    | Term.Int(n, id) ->
        let value = Int(n)
        returnValue id value state
    | Lambda(func, id) ->
        let value = Closure(func, state.Env)
        returnValue id value state
    | Let(name, rhs, body, termId) ->
        let values = evaluateSimple rhs state
        let addr = (name, termId)
        let env = Map.add name addr state.Env
        let store = addManyToStore addr values state.Store

        let newState =
            { state with
                  Term = body
                  Env = env
                  Store = store }

        Seq.singleton newState
    | LetCall(name, func, arg, body, termId) ->
        let funcVals = evaluateSimple func state
        let argVals = evaluateSimple arg state
        performCall name body termId funcVals argVals state

let inject term =
    { Term = term
      Env = Map.empty
      Store = Map.empty
      ContStore = Map.empty
      ContAddress = contAddressHalt }

let run term =
    let state = inject term

    let rec loop seen todo =
        if Seq.isEmpty todo then
            seen
        else
            let state = Seq.head todo
            let rest = Seq.tail todo
            if Set.contains state seen then
                loop seen rest
            else
                printfn "%A\n----------------" state
                let newStates = step state
                let seen = Set.add state seen
                let todo = Seq.append newStates rest
                loop seen todo

    loop Set.empty (Seq.singleton state)

//////////////////////////////////
/// Examples

/// Works for this example
/// a -> {1}
/// b -> {2}

/// (let ((id (lambda (x) x)))
///     (let ((a (id 1)))
///         (let ((b (id 2)))
///             b)))
let term1 =
    Let("id", Lambda(("x", Variable("x", 0)), 1),
      LetCall("a", Variable("id", 2), Term.Int(1, 3),
        LetCall("b", Variable("id", 4), Term.Int(2, 5),
            Variable("b", 6), 7), 8), 9)

/// Fails for this example
/// a -> {1, 2}
/// b -> {1, 2}

/// (let ((id (lambda (x) x)))
///     (let ((call-id (lambda (y) y)))
///         (let ((a (call-id 1)))
///             (let ((b (call-id 2)))
///                 b))))
let term2 =
    Let("id", Lambda(("x", Variable("x", 0)), 1),
      Let("call-id",
          Lambda(("y",
                  LetCall("z", Variable("id", 2), Variable("y", 3),
                          Variable("z", 4), 5)), 6),
            LetCall("a", Variable("call-id", 7), Term.Int(1, 8),
              LetCall("b", Variable("call-id", 9), Term.Int(2, 10),
                Variable("b", 11), 12), 13), 14), 15)