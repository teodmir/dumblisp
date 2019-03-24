namespace Dumblisp

module Eval =
  open Types
  open Parsers
  open FParsec
  open Primitives

  let bindVars (e : LispEnv) (binds : list<(string * LispVal)>) : LispEnv =
    List.fold (fun envSt x -> lispDefine envSt (fst x) (snd x) |> fst ) e binds

  let isAtom = function
  | Atom _ -> true
  | _ -> false

  let makeFunc varargs env parameters body =
    if List.forall isAtom parameters
    then
      match body with
      | [] -> raise <| LispBadFormException "Empty definitions are not allowed"
      | _::_ -> LispFunc { parameters = (List.map showVal parameters);
                           vararg = varargs;
                           body = body;
                           closure = env }
    else raise <| LispBadFormException "Expected identifiers in define"

  let makeNormalFunc = makeFunc None
  let makeVarArgs = showVal >> Some >> makeFunc

  // Run eval on all arguments while ensuring that the environment is
  // passed from left to right.
  // While it may be tempting to implement this in terms of foldBack
  // because of the single element appending, it would break the
  // left-to-right evaluation order which is necessary when side
  // effects are used.
  let rec evalSequentially (l : list<LispVal>) (e : LispEnv)
          : (LispEnv * list<LispVal>) =
      List.fold (fun acc x -> let res = (eval (fst acc) x)
                              let newEnv = fst res
                              let newRes = snd res
                              let oldList = snd acc
                              in (newEnv, oldList @ [newRes]))
                (e, [])
                l

  and eval (env : LispEnv) (value : LispVal) : EnvValPair =
    match value with
    // These evaluate to themselves.
    | String _ | Number _ | Bool _ -> (env, value)
    // Variable lookup
    | Atom id -> (env, getVar env id)
    // Quoted expressions: a special case of 2 element lists.
    // "Unquotes" the expression.  Cannot be implemented as a
    // primitive since that would force recursive evaluation according
    // to the standard list format.
    | List (Atom "quote" :: args) ->
        match args with
        | [expr] -> (env, expr)
        | _ -> raise <| LispNumArgsException "quote: expected single argument"
    // "if" has to be a special form: otherwise, due to eager
    // evaluation, we end up evaluating both branches
    | List (Atom "if" :: args) ->
        match args with
        | [pred; expr1; expr2] ->
            let (newEnv, predRes) = eval env pred
            if lispTrue predRes
            then eval newEnv expr1
            else eval newEnv expr2
        | _ -> raise <| LispNumArgsException
                        "if: Invalid arguments: expected (if pred expr1 expr2)"
    | List [Atom "set!"; Atom var; form] ->
        eval env form |> fun x -> setVar (fst x) var (snd x)
    | List [Atom "define"; Atom var; form] ->
        eval env form |> fun x -> lispDefine (fst x) var (snd x)
    | List (Atom "define" :: List (Atom var :: prms) :: body) ->
        makeNormalFunc env prms body |> lispDefine env var
    | List (Atom "define" :: DottedList (Atom var :: prms, varargs) :: body) ->
        makeVarArgs varargs env prms body |> lispDefine env var
    | List (Atom "lambda" :: List prms :: body) ->
        makeNormalFunc env prms body |> fun x -> (env, x)
    | List (Atom "lambda" :: DottedList (prms, varargs) :: body) ->
        makeVarArgs varargs env prms body |> fun x -> (env , x)
    | List (Atom "lambda" :: (Atom _ as varargs) :: body) ->
        makeVarArgs varargs env [] body |> fun x -> (env, x)
    | List ((f :: args) as execForm) ->
        evalSequentially execForm env |> fun x ->
          match x with
          | (env', func :: args') -> (fst x, apply env func args')
          | _ -> failwith "Invalid program state"
    | _ -> raise <| LispTypeException "Invalid Lisp form"

  and apply (env : LispEnv) (func : LispVal) (args : list<LispVal>) : LispVal =
    match func with
    | PrimitiveFunc f -> f args
    | LispFunc { parameters = prms; vararg = varargs; body = body; closure = closure } ->
        if List.length prms <> List.length args && varargs = None
        then raise <| LispNumArgsException "Invalid number of arguments"
        else
          let remainingArgs = List.skip (List.length prms) args
          let bindVarArgs arg env =
            match arg with
            | None -> env
            | Some x -> bindVars env [(x, List remainingArgs)]
          // Let the bindings of e1 shadows the bindings of e2; this
          // allows us to give the closure priority while using the
          // main environment as a "fallback"
          // let mergeEnvs (e1 : LispEnv) (e2 : LispEnv) : LispEnv =
          //   Map.fold (fun acc key value -> Map.add key value acc) e2 e1

          // Evaluate the body in a new environment where the
          // parameters substituted for the arguments
          bindVars env (List.zip prms args) |> bindVarArgs varargs
          |> evalSequentially body |> snd |> List.last
    | _ -> raise <| LispTypeException "apply: invalid form"
