module rec Anf

/// let x = e1 e2 in e3
/// x
/// lambda x: e1
/// number

type TermId = int

type Term =
    | LetCall of name: string * func: string * arg: string * body: Term * id: TermId
    | LetFunc of name: string * func: Lambda * body: Term * id: TermId
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


let step state =
    match state.Term with
    | Variable(name = name) ->
        let value = state.Env.[name]
        continueWithVal value state
    | LetCall(name, func, parameter, body) ->
        let (Closure((arg, funcBody), funcEnv)) = state.Env.[func]
        let parameter' = state.Env.[parameter]
        let stackEntry = name, body, state.Env
        let funcEnv' = Map.add arg parameter' funcEnv
        [ { Term = funcBody
            Env = funcEnv'
            Stack = stackEntry :: state.Stack } ]

let run term =

    term
