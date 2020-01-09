module rec Anf

/// let x = e1 e2 in e3
/// x
/// lambda x: e1

type Term =
    | LetCall of name : string * func : string * arg : string * body : Term
    | LetFunc of name : string * func : Lambda * body : Term
    | Variable of name : string

// Respresents argument and body of a function.
type Lambda = string * Term

// Maps from variable name to values.
type Environment = Map<string, Value>

// The only value for now is closure.
type Value = Closure of Lambda * Environment

// Continuation stack.
type StackEntry = string * Term * Environment

type Stack = list<StackEntry>

type State = {
    Term : Term
    Env : Environment
    Stack : Stack
}

/// Apply the value to the top of the continuation stack
let continueWithVal value state =
    match state.Stack with
    | (var, term, env) :: rest ->
        let env2 = Map.add var value env
        [{Term = term; Env = env2; Stack = rest}]
    | _ -> []

let step state =
    match state.Term with
    | Variable(name) ->
        let value = state.Env.[name]
        continueWithVal value state
    | LetCall(name, func, parameter, body) ->
        let (Closure((arg, funcBody), funcEnv)) = state.Env.[func]
        let parameter' = state.Env.[parameter]
        let stackEntry = name, body, state.Env
        let funcEnv' = Map.add arg parameter' funcEnv
        [{Term = funcBody; Env = funcEnv'; Stack = stackEntry :: state.Stack}]

let run term =

    term