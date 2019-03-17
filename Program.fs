namespace DumbLisp

open FParsec.Internals
open FParsec.Primitives
open FParsec.CharParsers

module Program =

  type LispVal =
    | Atom of string
    | List of LispVal list
    | DottedList of LispVal list * LispVal
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
  // the mutually recursive calls
  let parseExpr, parseExpr' = createParserForwardedToRef ()

  // Lists; expressions separated by arbitrary amount of whitespace
  let parseNormalList : Parser<LispVal, unit> = parse {
    let! l = sepBy parseExpr spaces
    do! spaces
    return List l
    }

  let parseDottedList = parse {
    let! init = parseExpr .>> spaces |> many
    let! last = pchar '.' >>. spaces >>. parseExpr
    return DottedList (init, last)
    }

  // Parse either normal lists or dotted lists.
  // let parseList = parse {
  //   do! skipChar '(' .>> spaces
  //   // Backtrack if parsing a normal list fails. Necessary since we
  //   // may end up with a partially consumed stream if parseNormalList
  //   // finds a dot.
  //   // let! x = attempt parseNormalList <|> parseDottedList
  //   let! x = parseNormalList
  //   do! skipChar ')' .>> spaces
  //   return x
  //   }

  // fixme
  let parseList : Parser<LispVal, unit> = (pchar '(' >>. spaces >>. sepEndBy parseExpr spaces1 .>> spaces .>> pchar ')' .>> spaces) |>> List

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
    match run parseExpr input with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> String errorMsg

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
        unwords (List.map showVal init) + " . " + showVal last
        |> listWrap

  let printVal = showVal >> printfn "%s"

  // Type alias: Lisp functions take an arbitary amount of Lisp values
  // and return another Lisp value
  type LispFunction = LispVal list -> LispVal

  let arithmeticOp (op : int -> int -> int) (argList : LispVal list) =
    let getNum (v : LispVal) =
      match v with
      | Number v -> v
      | _ -> 0
    in List.map getNum argList |> List.reduce op |> Number

  let primitives : Map<string, LispFunction> =
    Map.ofList [
      ("+", arithmeticOp (+))
      ("-", arithmeticOp (-))
      ("*", arithmeticOp (*))
      ("/", arithmeticOp (/))
    ]

  let apply func args =
    match Map.tryFind func primitives with
    | Some f -> f args
    | None -> Bool false

  let rec eval (value : LispVal) =
    match value with
    // These evaluate to themselves.
    | String _ | Number _ | Bool _ -> value
    // Quoted expressions: a special case of 2 element lists.
    // "Unquotes" the expression.
    | List [Atom "quote"; value'] -> value'
    | List (Atom f :: args) -> List.map eval args |> apply f

  [<EntryPoint>]
  let main argv =
    argv.[0] |> readExpr |> eval |> printVal
    0
