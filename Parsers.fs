namespace Dumblisp

open FParsec.Internals
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open Types

module Parsers =
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

  // Useful for parsing files
  let parseManyExprs = spaces >>. sepEndBy parseExpr spaces .>> eof
