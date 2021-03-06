module Cps

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
    | Call of CallInfo
    | Letrec of label : Label * bindings : (Variable * Simple) list * body : Complex

and Lambda = Label * Variable list * Complex

and CallInfo = Label * Simple * Simple list

type Program = Lambda

// Contour or dynamic frame
