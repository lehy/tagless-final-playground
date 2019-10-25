module type S = sig
  type module_
  type expression
  val module_ : string -> expression list -> module_
  val assign : string -> expression -> expression
  val apply : expression -> expression list -> expression
  val variable : string -> expression
  val function_ : string list -> expression list -> expression
  val return : expression -> expression
  val plus : expression -> expression -> expression
  val int : int -> expression
end
              
let test (type module_) (module B : S with type module_ = module_) =
  B.module_ "" [
      B.assign "f" @@ B.function_ ["x"] [B.return @@ B.plus (B.variable "x") (B.int 1)];
      B.assign "y" @@ B.apply (B.variable "f") [B.int 42];
      B.apply (B.variable "print") [B.variable "y"]
    ]

module Interpret = struct
  module Value = struct
    type t = {
        float : unit -> float;
        string : unit -> string; 
        apply : t list -> t
      }
    let none = {
        float = (fun () -> failwith "cannot convert None to float");
        string = (fun () -> "None");
        apply = (fun _ -> failwith "object of type NoneType cannot be called")
      }
    let int i = {
        float = (fun () -> Float.of_int i);
        string = (fun () -> string_of_int i);
        apply = (fun _ -> failwith "object of type int cannot be called")
      }
    let float f = {
        float = (fun () -> f);
        string = (fun () -> string_of_float f);
        apply = (fun _ -> failwith "object of type float cannot be called")
      }
  end
  module Builtin = struct
    let print args =
      let sep = " " in
      begin match args with
      | [] -> ()
      | t::q ->
         Printf.printf "%s" (t.Value.string ());
         List.iter (fun e -> Printf.printf "%s%s" sep (e.Value.string ())) q
      end;
      Printf.printf "\n";
      Value.none
  end
                 
  module Environment = struct
    module M = Map.Make(String)
    type t = Value.t M.t
    let empty = M.empty
    let add env k v = M.add k v env
    let get env k =
      match M.find_opt k env with
      | None -> failwith ("no such variable: " ^ k)
      | Some v -> v

    let init () =
      let open Value in
      add empty "print" {
          float = (fun () -> failwith "cannot convert object of type function to float");
          string = (fun () -> "<build-in function print>");
          apply = Builtin.print
        }
  end
  module Expression = struct
    type v = { value : Value.t; env : Environment.t; is_return : bool }
    type t = Environment.t -> v
  end
  type expression = Expression.t
                  
  type module_ = Environment.t -> string * Environment.t
               
  let module_ name expressions env =
    name, List.fold_left (fun env e -> (e env).Expression.env) env expressions
  let assign name expr : Expression.t = fun env ->
    let ee = expr env in
    let env = Environment.add ee.Expression.env name ee.Expression.value in
    Expression.{ value=ee.value; env; is_return=false }
  let apply f_expr arg_exprs : Expression.t = fun env ->
    let Expression.{value=f; env; _} = f_expr env in
    let values, env =
      List.fold_left (fun (values, env) a ->
          let ae = a env in ae.Expression.value::values, ae.Expression.env)
        ([], env) arg_exprs in
    Expression.{value=f.Value.apply values;
                env;
                is_return=false}
  let variable name : Expression.t = fun env ->
    Expression.{ value=Environment.get env name; env; is_return=false }
    
  let function_ arg_names exprs : Expression.t = fun env ->
    let value = Value.{
          float = (fun () -> failwith "object of type function cannot be called");
          string = (fun () -> "<function>");
          apply = fun arg_values ->
                  let env =
                    List.fold_left2 Environment.add env arg_names arg_values
                  in
                  let ee =
                    let exception Return of Expression.v in
                    try
                      List.fold_left (fun {Expression.env; _} e ->
                          let ee = e env in
                          if ee.Expression.is_return then
                            raise (Return ee)
                          else ee)
                        Expression.{value=Value.none; env; is_return=false} exprs
                    with Return ee -> ee
                  in ee.value
                } in
    Expression.{value; env; is_return=false}
  let return expr : Expression.t = fun env ->
    Expression.{ (expr env) with is_return=true }
  let plus a_expr b_expr : Expression.t = fun env ->
    let open Expression in
    let {value=a; env; _} = a_expr env in
    let {value=b; env; _} = b_expr env in
    let value = Value.float (a.Value.float () +. b.Value.float ()) in
    Expression.{value; env; is_return=false}
  let int i : Expression.t = fun env ->
    let value = Value.int i in
    Expression.{value; env; is_return=false}

  let eval x = x (Environment.init ())
end
module DummyInterpret = (Interpret : S);;

let test1 (type module_) (module B : S with type module_ = module_) =
  B.module_ "main" [
      B.apply (B.variable "print") [B.int 42]
    ]

let test2 (type module_) (module B : S with type module_ = module_) =
  B.module_ "" [
      B.assign "f" @@ B.function_ ["x"] [B.return @@ B.plus (B.variable "x") (B.int 1)];
      B.assign "y" @@ B.apply (B.variable "f") [B.int 42];
      B.apply (B.variable "print") [B.variable "y"]
    ]

let _ = Interpret.eval (test1 (module Interpret));;
let _ = Interpret.eval (test2 (module Interpret));;
(*  TODO: implement builtin print()  *)

#require "angstrom"

module Parse = struct
  open Angstrom

  let parse (type t) s (module B : S with type module_ = t) =
    let space = choice [char ' '; char '\t'] in
    let spaces = skip_many space in
    let spaces_count = many space >>| List.length in
    let spaces1 = skip_many1 space in
    let spaces_exact n = count n space in
    let parse_ident =
      take_while1 (function '_' | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false) >>= fun first ->
      take_while (function '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false) >>| fun second ->
      first ^ second
    in
    let parse_ident_list indent =
      (*  TODO: allow line returns with appropriate indentation  *)
      sep_by (spaces *> char ',' *> spaces) parse_ident
    in
    let rec parse_def base_indent : B.expression Angstrom.t =
      string "def" *> spaces1 *> parse_ident >>= fun function_name ->
      char '(' *> parse_ident_list base_indent <* char ')' >>= fun params ->
      spaces *> char ':' *> spaces *> choice [
        (parse_expr base_indent >>| fun x -> [x]);
        many1 end_of_line *> spaces_count >>= fun indent ->
        if indent <= base_indent then fail (Printf.sprintf "expected indentation of at least %d" (base_indent+1))
        else begin
          parse_expr indent >>= fun first_expr ->
          many (spaces_exact indent *> parse_expr indent) >>| fun rest_exprs ->
          first_expr::rest_exprs
        end
      ] >>| fun body -> B.assign function_name (B.function_ params body)
    and parse_assign indent =
      parse_ident <* spaces <* char '=' <* spaces >>= fun lident -> parse_expr indent >>| B.assign lident
    and parse_callable_expr indent =
      choice [
        parse_ident >>| B.variable;
        char '(' *> spaces *> parse_expr indent <* spaces <* char ')'
      ]
    and parse_call indent =
      parse_callable_expr indent <* spaces <* char '(' >>= fun func ->
      parse_expr_list indent <* char ')' >>| B.apply func
    and parse_expr_list indent =
      sep_by (spaces *> char ',' *> spaces) (parse_expr indent)
    and parse_expr indent : B.expression Angstrom.t =
      choice ~failure_msg:(Printf.sprintf "expected expression indented with %d spaces" indent) [
        parse_def indent;
        parse_assign indent;
        parse_call indent
      ]
    in
    let parse_module =
      many (parse_expr 0) <* end_of_input >>| B.module_ ""
    in
    parse_string parse_module s
end

let test_string = {|
def f(x):
  return x + 1

y = f(42)

print(y)
|}

let result_map f x = match x with
  | Error e -> Error e
  | Ok res -> Ok (f res)

let _ = result_map Interpret.eval (Parse.parse test_string (module Interpret))
