namespace DumbLisp

open FParsec.CharParsers

module Program =

  let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

  [<EntryPoint>]
  let main _ =
    test pfloat "7.25"
    0
