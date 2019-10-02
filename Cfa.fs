module Cfa

type Expression =
    | Reference of label : int * variable : string
    | Lambda of label : int * args : string list * Call
and Call = Call of label : int * args : Expression list