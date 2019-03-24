namespace Dumblisp

module Primitives =
  open Types

  let unwords (ws : string list) =
    // Helper function to allow tail recursion (at the cost of readability)
    let rec unwords' ws acc =
      match ws with
      | [] -> acc
      | [w] -> acc + w
      | w::ws' -> unwords' ws' (acc + w + " ")
    in unwords' ws ""

  // Print the results of evaluation.
  let rec showVal value : string =
    let listWrap (s : string) = "(" + s + ")"
    match value with
    | String s -> "\"" + s + "\""
    | Atom a -> a
    | Number x -> string x
    | Bool true -> "#t"
    | Bool false -> "#f"
    | List xs -> List.map showVal xs |> unwords |> listWrap
    | DottedList (init, last) ->
        unwords (List.map showVal init) + " . " + showVal last |> listWrap
    | (PrimitiveFunc _) -> "<primitive>"
    | LispFunc { parameters = args; vararg = rest } ->
        let varargString =
          match rest with
          | Some a -> "." + a
          | None -> ""
        sprintf "(lambda (%s%s) ...)"
          (unwords args)
          varargString

  // Helper function to extract a number from a LispVal; throws an exception
  // if it isn't a Number
  let getNum (v : LispVal) : int =
    match v with
    | Number x -> x
    | _ -> raise <| LispTypeException "Not a number"

  // Generalization of the basic arithmetic operations in Lisp: used
  // to partially apply and obtain the concrete functions +, -, * and /
  let arithmeticOp (op : int -> int -> int) (argList : list<LispVal>) : LispVal =
    match argList with
    | [] | [_] -> raise <| LispNumArgsException "Invalid number of arguments"
    | _ -> List.map getNum argList |> List.reduce op |> Number

  let numBoolBinOp (op : int -> int -> bool) (argList : list<LispVal>) =
    match argList with
    | [x; y] -> op (getNum x) (getNum y) |> Bool
    | _ -> raise <| LispNumArgsException "Invalid amount of arguments"

  // Internal function for determining truthiness with Scheme semantics:
  // that is, anything that is not #f
  let lispTrue (x : LispVal) : bool =
    match x with
    | Bool false -> false
    | _ -> true

  // Used for AND, OR: extend the binary operator op to work on lists while
  // preserving the semantics
  let LispLogicalOp (op : LispVal -> LispVal -> LispVal)
                    (argList : list<LispVal>) =
    match argList with
    | [] -> raise <| LispNumArgsException "Invalid amount of arguments"
    | _ -> List.reduce op argList

  // Evaluate the elements of argList and return the first one that is true.
  let lispOr argList =
    let lispBinOr (x : LispVal) (y : LispVal) =
      match x with
      | _ when lispTrue x -> x
      | _ -> y
    in LispLogicalOp lispBinOr argList

  // Evaluate the elements of argList until one is false.
  // If all of them are true, return the last one.
  let lispAnd argList =
    let lispBinAnd (x : LispVal) (y : LispVal) =
      match x with
      | _ when lispTrue x -> y
      | _ -> Bool false
    in LispLogicalOp lispBinAnd argList

  let lispNot (argList : list<LispVal>) : LispVal =
    match argList with
    | [x] -> if lispTrue x then Bool false else Bool true
    | _ -> raise <| LispNumArgsException "Invalid number of arguments"

  // The first element of a list
  let lispCar (argList : list<LispVal>) : LispVal =
    match argList with
    | [List (x::_)] -> x
    | [List []] -> raise <| LispTypeException "car: Empty list"
    | [DottedList (x::_, _)] -> x
    | [_] -> raise <| LispTypeException "Invalid type"
    | [] | _::_ -> raise <| LispNumArgsException "Invalid number of arguments"

  // The tail of a list
  let lispCdr (argList : list<LispVal>) : LispVal =
    match argList with
    | [List (_::xs)] -> List xs
    | [List []] -> raise <| LispTypeException "cdr: Empty list"
    | [DottedList ([_], x)] -> x
    | [DottedList ((_::xs), x)] -> DottedList (xs, x)
    | [_] -> raise <| LispTypeException "Invalid type"
    | [] | _::_ -> raise <| LispNumArgsException "Invalid number of arguments"

  let lispCons (argList : list<LispVal>) : LispVal =
    match argList with
    | [x; List xs] -> List (x::xs)
    | [x; DottedList (xs, final)] -> DottedList (x::xs, final)
    | [expr1; expr2] -> DottedList ([expr1], expr2)
    | [_] -> raise <| LispTypeException "Invalid type"
    | [] | _::_ -> raise <| LispNumArgsException "Invalid number of arguments"

  let lispList (argList : list<LispVal>) : LispVal = List argList

  type EnvValPair = LispEnv * LispVal

  let getVar (env : LispEnv) (var : string) =
    match Map.tryFind var env with
    | Some x -> x
    | None -> raise <| LispUnboundException ("Variable is unbound: " + var)

  let setVar (env : LispEnv) (var : string) (def : LispVal) : EnvValPair =
    if Map.containsKey var env
    then (Map.add var def env, List [])
    else raise <| LispUnboundException "Attempted to set unbound var"

  // Add a new, or replace, a binding in the environment
  let lispDefine (env : LispEnv) (var : string) (def : LispVal)
      : EnvValPair =
    (Map.add var def env, List [])

  let primitives : list<(string * (LispVal list -> LispVal))> =
     [
      ("+", arithmeticOp (+))
      ("-", arithmeticOp (-))
      ("*", arithmeticOp (*))
      ("/", arithmeticOp (/))
      ("=", numBoolBinOp (=))
      ("/=", numBoolBinOp (<>))
      (">", numBoolBinOp (>))
      (">=", numBoolBinOp (>=))
      ("<", numBoolBinOp (<))
      ("<=", numBoolBinOp (<=))
      ("or", lispOr)
      ("and", lispAnd)
      ("not", lispNot)
      ("car", lispCar)
      ("cdr", lispCdr)
      ("cons", lispCons)
      ("list", lispList)
    ]

  let primitiveBindings : LispEnv =
    let mapSnd f x = (fst x, f (snd x))
    List.map (mapSnd PrimitiveFunc) primitives |> Map.ofList
