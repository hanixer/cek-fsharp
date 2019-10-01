module Cek

type Variable = string
type Term =
    | Reference of Variable
    | Lambda of Lambda
    | Call of Term * Term
and Lambda = Variable * Term

type Value = Closure of Lambda * Environment
and Environment = Map<Variable, Value>
type Continuation =
    | Done
    | Argument of Term * Environment * Continuation
    | Function of Lambda * Environment * Continuation
type State = Term * Environment * Continuation

let r = Reference
let l a b = Lambda(a, b)
let c a b= Call(a, b)
let cl a b = Closure(a, b)

let lookup var env =
    Map.find var env

let step state =
    let (term, env, cont) = state
    match term, cont with
    | Reference(var), _ ->
        match lookup var env with
        | Closure(lambda, env2) -> (Lambda lambda, env2, cont)
    | Call(a, b), _ ->
        (a, env, Argument(b, env, cont))
    | Lambda lambda, Argument(arg, env2, cont2) ->
        (arg, env2, Function(lambda, env, cont2))
    | Lambda lambda, Function((var, body), env2, cont2) ->
        let env3 = Map.add var (Closure(lambda, env)) env2
        (body, env3, cont2)
    | _ -> state

let isFinal =
    function
    | (Lambda _, _, Done) -> true
    | _ -> false

let evaluate term =
    let rec loop state =
        if isFinal state then
            printfn "Final state: %A" state
        else
            let next = step state
            printfn "State: %A\n" state
            loop next

    let env = Map.empty
    let cont = Done
    loop (term, env, cont)
