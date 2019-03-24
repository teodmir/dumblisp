namespace Dumblisp

open FParsec.Internals
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error
open System

module Program =
  open Parsers
  open Types
  open Dumblisp.Primitives
  open Eval
  open System.IO

  let readExpr input =
    match run parseInteractive input with
    | Success (result, _, _) -> result
    | Failure (errorMsg,  _, _) -> String errorMsg

  // Remove characters [0..31], representing control chars
  let removeControlChars = String.filter (fun c -> int c > 31)

  // Not sure why the newline needs to be manually appended. The null
  // checking is for the EOF character, which makes ReadLine() return
  // null. Consider returning early
  let readPrompt prompt =
    printf "%s" prompt
    match Console.ReadLine() with
    | null -> printf "\n" ; None
    | s -> removeControlChars s + "\n" |> Some

  let incompleteInput (s : string) : bool =
    let occurs (c : char) = String.filter ((=) c) >> String.length
    let openCount = occurs '(' s
    let closeCount = occurs ')' s
    openCount > closeCount || s.Trim() = ""


  let printVal = showVal >> printfn "%s"

  let rec REPL env inputSt =
    match readPrompt (if inputSt = "" then "Dumblisp> " else "> ") with
      // EOF aborts the current input
    | None -> REPL env ""
      // Exit if ";" is entered
    | Some s when s.Trim() = ";" -> ()
    | Some s ->
        let newInput = inputSt + s
        if incompleteInput newInput
        then REPL env newInput
        else
          readExpr newInput |>
            fun expr ->
            // Convert exceptions to options
            let maybeResult : EnvValPair option =
              try Some <| eval env expr
              with
              | LispNumArgsException msg
              | LispTypeException msg
              | LispBadFormException msg
              | LispUnboundException msg ->
                  printfn "Lisp exception: %s" msg ; None
            in
            match maybeResult with
            // If evaluation succeeds, pass the new environment
            // recursively and reset the current REPL input; otherwise
            // just restart with the old environment
            | Some expr -> printVal (snd expr) ; REPL (fst expr) ""
            | None -> REPL env ""

  // Evaluates all expressions and return the value of the last one.
  let evalFile (f : string) (e : LispEnv) : Result<LispVal, string>  =
    match run parseManyExprs (File.ReadAllText(f)) with
    | Success (result, _, _) ->
        try
          evalSequentially result e
        with
        | LispNumArgsException msg
        | LispTypeException msg
        | LispBadFormException msg
        | LispUnboundException msg -> failwith msg
        |> snd |> List.last |> Result.Ok
    | Failure (errmsg, _, _) -> Result.Error errmsg

// TODO:
  // - More comments
  // - generalize equality
  // - Error check for well formed define
  // - Closure is never used
  [<EntryPoint>]
  let main argv =
    if argv.Length > 0
    then
      match evalFile argv.[0] primitiveBindings with
      | Result.Ok value -> printfn "%s" (showVal value) ; 0
      | Result.Error msg -> printfn "%s" msg ; 1
    else
      printfn "Dumblisp REPL: enter ';' to exit"
      REPL primitiveBindings ""
      0
