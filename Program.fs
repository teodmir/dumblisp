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
    let! l = sepBy parseExpr spaces1
    return List l
    }

  let parseDottedList = parse {
    let! init = parseExpr .>> spaces |> many
    let! last = pchar '.' >>. spaces >>. parseExpr
    return DottedList (init, last)
    }

  // Parse either normal lists or dotted lists.
  let parseList = parse {
    do! skipChar '('
    let! x = attempt parseNormalList <|> parseDottedList
    do! skipChar ')'
    return x
    }

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
      | w::ws' -> unwords' ws' (w + " " + acc)
    in unwords' ws ""

  let listWrap (s : string) = "(" + s + ")"

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

  let readExpr input =
    match run parseExpr input with
    | Success (result, _, _) -> printfn "Success: %s" <| showVal result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

  [<EntryPoint>]
  let main argv =
    readExpr argv.[0]
    0
