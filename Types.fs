namespace Dumblisp

module Types =

  type LispVal =
    | Atom of string
    | List of list<LispVal>
    | DottedList of list<LispVal> * LispVal
    | Number of int
    | String of string
    | Bool of bool
    | PrimitiveFunc of (list<LispVal> -> LispVal)
    | LispFunc of LispFunc

  and LispFunc = { parameters : list<string>; vararg : Option<string>
                   body : list<LispVal>; }

  type LispEnv = Map<string, LispVal>

  // Accessing unbound symbols
  exception LispUnboundException of string
  // Invalid amount of arguments
  exception LispNumArgsException of string
  // Incompatible types
  exception LispTypeException of string
  // Miscellaneous errors not covered by the above, usually for
  // special forms with complicated evaluation rules
  exception LispBadFormException of string
