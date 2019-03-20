namespace DumbLisp

open FParsec.Internals
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error
open System

module Program =

  type LispVal =
    | Atom of string
    | List of list<LispVal>
    | DottedList of list<LispVal> * LispVal
    | Number of int
    | String of string
    | Bool of bool

  // Parse non-letter characters allowed in Scheme atoms.
  let symbol : Parser<char, unit> = anyOf "!#$%&|*+-/:<=>?@^_~"

  // Parse strings; a sequence of characters enclosed in double quotes.
  let parseString : Parser<LispVal, unit> = parse {
    let! s = between (pchar '"') (pchar '"') (manyChars (noneOf "\""))
    return String s
  }

  // Parse atoms (Lisp identifiers); treat the constants #t and #f
  // as special cases, using them as booleans
  let parseAtom : Parser<LispVal, unit> = parse {
    // The first character may not be a digit.
    let! first = letter <|> symbol
    let! rest = manyChars (letter <|> digit <|> symbol)
    let atom = string first + rest
    return match atom with
           | "#t" -> Bool true
           | "#f" -> Bool false
           | _ -> Atom atom
    }

  let parseNumber : Parser<LispVal, unit> = parse {
    let! x = pint32
    return Number x
    }

  // Forward declaration of the main parser function; necessary due to
  // the mutual calls
  let parseExpr, parseExpr' = createParserForwardedToRef ()

  // Lists; expressions separated by arbitrary amount of whitespace,
  // enclosed in brackets
  let parseNormalList : Parser<LispVal, unit> =
    pchar '(' >>. spaces >>.
    sepEndBy parseExpr spaces .>>
    pchar ')' |>> List

  // Dotted lists: Lisp lists of the form (a b . c) Parses up the
  // prefix to the dot as a normal list, and the suffix is parsed as a
  // single expression
  let parseDottedList = parse {
    do! pchar '(' >>. spaces
    let! init = sepEndBy parseExpr spaces1
    let! last = pchar '.' .>> spaces1 >>. parseExpr .>> spaces
    do! skipChar ')'
    return DottedList (init, last)
    }

  let parseList : Parser<LispVal, unit> =
    // Try parsing a normal list; on failure, backtrack and parse as a
    // dotted list
    attempt parseNormalList <|> parseDottedList

  // Quoted expressions: store the whole expression that succeeds the
  // quote character. Syntactic sugar for the special form (quote x).
  let parseQuoted = parse {
    do! skipChar '\''
    let! x = parseExpr
    return List [Atom "quote"; x]
    }

  // High level expression parser, combined from the other parsers.
  parseExpr' :=
    parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseList

  // For user interaction: ignore leading and trailing whitespace, but
  // ensure there are no other trailing characters as well
  let parseInteractive = spaces >>. parseExpr .>> spaces .>> eof

  let unwords (ws : string list) =
    // Helper function to allow tail recursion (at the cost of readability)
    let rec unwords' ws acc =
      match ws with
      | [] -> acc
      | [w] -> acc + w
      | w::ws' -> unwords' ws' (acc + w + " ")
    in unwords' ws ""

  let listWrap (s : string) = "(" + s + ")"

  let readExpr input =
    match run parseInteractive input with
    | Success (result, _, _) -> result
    | Failure (errorMsg,  _, _) -> String errorMsg

  // Print the results of evaluation.
  let rec showVal value : string =
    match value with
    | String s -> "\"" + s + "\""
    | Atom a -> a
    | Number x -> string x
    | Bool true -> "#t"
    | Bool false -> "#f"
    | List xs -> List.map showVal xs |> unwords |> listWrap
    | DottedList (init, last) ->
        unwords (List.map showVal init) + " . " + showVal last |> listWrap

  let printVal = showVal >> printfn "%s"

  type LispFunction = LispVal list -> LispVal

  // Helper function to extract a number from a LispVal; throws an exception
  // if it isn't a Number
  let getNum (v : LispVal) : int =
    match v with
    | Number x -> x
    | _ -> failwith "Not a number"

  // Generalization of the basic arithmetic operations in Lisp: used
  // to partially apply and obtain the concrete functions +, -, * and /
  let arithmeticOp (op : int -> int -> int) (argList : list<LispVal>) : LispVal =
    match argList with
    | [] | [_] -> failwith "Invalid number of arguments"
    | _ -> List.map getNum argList |> List.reduce op |> Number

  let numBoolBinOp (op : int -> int -> bool) (argList : list<LispVal>) =
    match argList with
    | [x; y] -> op (getNum x) (getNum y) |> Bool
    | _ -> failwith "Invalid amount of arguments"

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
    | [] -> failwith "Invalid amount of arguments"
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
    | _ -> failwith "Invalid number of arguments"

  // The first element of a list
  let lispCar (argList : list<LispVal>) : LispVal =
    match argList with
    | [List (x::_)] -> x
    | [DottedList (x::_, _)] -> x
    | [_] -> failwith "Invalid type"
    | [] | _::_ -> failwith "Invalid number of arguments"

  // The tail of a list
  // TODO: error for empty list
  let lispCdr (argList : list<LispVal>) : LispVal =
    match argList with
    | [List (_::xs)] -> List xs
    | [List []] -> failwith "cdr: "
    | [DottedList ([_], x)] -> x
    | [DottedList ((_::xs), x)] -> DottedList (xs, x)
    | [_] -> failwith "Invalid type"
    | [] | _::_ -> failwith "Invalid number of arguments"

  let lispCons (argList : list<LispVal>) : LispVal =
    match argList with
    | [x; List xs] -> List (x::xs)
    | [x; DottedList (xs, final)] -> DottedList (x::xs, final)
    | [expr1; expr2] -> DottedList ([expr1], expr2)
    | [_] -> failwith "Invalid type"
    | [] | _::_ -> failwith "Invalid number of arguments"

  let lispList (argList : list<LispVal>) : LispVal = List argList

  let primitives : Map<string, LispFunction> =
    Map.ofList [
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

  let apply (func : string) (args : list<LispVal>) : LispVal =
    match Map.tryFind func primitives with
    | Some f -> f args
    | None -> failwith "Unknown primitive function"

  let rec eval (value : LispVal) =
    match value with
    // These evaluate to themselves.
    | String _ | Number _ | Bool _ -> value
    // Quoted expressions: a special case of 2 element lists.
    // "Unquotes" the expression.  Cannot be implemented as a
    // primitive since that would force recursive evaluation according
    // to the standard list format.
    | List (Atom "quote" :: args) ->
        match args with
        | [expr] -> expr
        | _ -> failwith "quote: expected single argument"
    // "if" has to be a special form: otherwise, due to eager
    // evaluation, we end up evaluating both branches
    | List (Atom "if" :: args) ->
        match args with
        | [pred; expr1; expr2] -> if lispTrue pred
                                  then eval expr1
                                  else eval expr2
        | _ -> failwith "if: Invalid arguments: expected (if pred expr1 expr2)"
    | List (Atom f :: args) -> List.map eval args |> apply f
    | badForm -> failwith "Invalid Lisp form"

  let readPrompt prompt = printf "%s" prompt ; Console.ReadLine()

  type MaybeLispVal = Redo | Done of LispVal

  let rec REPL () =
    match readPrompt "λ> " with
    | s when s.Trim() = ";" -> ()
    | s -> readExpr s |> eval |> printVal ; REPL ()


  // TODO: checking arglist arity is so common it probably should be abstracted
  // - More comments
  // - generalize equality
  // - Multiline REPL input
  // - user-created exceptions
  [<EntryPoint>]
  let main argv =
    printfn "%s\n%s" "dumblisp REPL" "Enter ';' to exit."
    REPL ()
    0
