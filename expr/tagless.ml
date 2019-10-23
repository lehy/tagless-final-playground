module Expr = struct
  type t =
    | Constant of float
    | Add of t * t
    | Neg of t
    | Mult of t * t
end;;

module type S = sig
  type t
  val constant : float -> t
  val add : t -> t -> t
  val neg : t -> t
  val mult : t -> t -> t
end;;

module type SS = sig
  include S
  val to_string : t -> string
end;;

module String = struct
  type t = string
  let constant x = Float.to_string x
  let add a b = Printf.sprintf "(%s + %s)" a b
  let neg a = Printf.sprintf "(- %s)" a
  let mult a b = Printf.sprintf "(%s * %s)" a b
  let to_string x = x
end;;
module Dummy = (String : S);;

let test (type t) (module B : S with type t = t) =
  B.(add
       (neg (constant 42.))
       (mult
          (neg (constant 3.))
          (neg
             (add
                (add
                   (constant 5.)
                   (constant 7.))
                (constant (-5.))))));;

(*  like String, but expand add a (add b c) to a + b + c *)
module String2 = struct
  type t = string
  let constant x = Float.to_string x
  let add a b =
    Printf.sprintf "%s + %s" a b
  let neg a = Printf.sprintf "- (%s)" a
  let mult a b = Printf.sprintf "(%s) * (%s)" a b
  let to_string x = x
end;;
module Dummy2 = (String2 : S);;

(*  distribute neg
    -(a + b) -> -a + -b
    -(a * b) -> -a * b
    -(-a) -> a
*)
module String3 = struct
  type context = Pos | Neg
  type t = context -> string
  let constant x = function
    | Pos -> Float.to_string x
    | Neg -> Float.to_string (-.x)
  let add a b ctx =
    Printf.sprintf "%s + %s" (a ctx) (b ctx)
  let neg a = function
    | Pos -> a Neg
    | Neg -> a Pos
  let mult a b = function
    | Pos -> Printf.sprintf "(%s) * (%s)" (a Pos) (b Pos)
    | Neg -> Printf.sprintf "(%s) * (%s)" (a Neg) (b Pos)
  let to_string x = x Pos
end;;
module Dummy3 = (String3 : S);;

String.to_string (test (module String));;
String2.to_string (test (module String2));;
String3.to_string (test (module String3));;

module Eval = struct
  type t = float
  let constant x = x
  let add a b = a +. b
  let mult a b = a *. b
  let neg a = -.a
  let eval x = x
end;;
module DummyEval = (Eval : S);;

Eval.eval (test (module Eval));;

module Parse = struct
    open Angstrom

    let sign = choice [string "+"; string "-"; return ""]
    let digits = take_while (function '0' .. '9' -> true | _ -> false)

    let float =
      lift4 (fun sign left dot right -> Float.of_string (sign ^ left ^ dot ^ right))
        sign digits (string ".") digits

    let skip_space = skip_while (function ' ' | '\t' | '\012' | '\015' -> true | _ -> false)

    (* let debug name x =
     *   Printf.eprintf "parsed %s: %s\n%!" name (B.to_string x);
     *   x
     * in *)

    (* let ( <?> ) p name =
     *   p <?> name >>| fun x ->
     *   Printf.eprintf "parsed %s: %s\n%!" name (B.to_string x);
     *   x
     * in *)

    let parse (type t) s (module B : SS with type t = t) =
      let expr_add = fix (fun expr_add ->
          let expr_mult = fix (fun expr_mult ->
              let expr_factor =
                (skip_space *>
                 choice ~failure_msg:"expecting ( or float" [
                   char '(' *> expr_add <* char ')';
                   float >>| B.constant
                 ] <* skip_space)
              in

              skip_space *>
              (choice [
                  expr_factor >>= fun first_factor ->
                  (many ((char '*') *> expr_factor)) >>| fun factors ->
                  List.fold_left B.mult first_factor factors
                ])
              <* skip_space)
          in

          skip_space *> expr_mult >>= fun first_term ->
          (many ((char '+') *> expr_mult)) >>| begin fun terms ->
            List.fold_left B.add first_term terms
          end <* skip_space)
      in
      let root = expr_add <* end_of_input in
      parse_string root s
end;;

let ( |> ) x f =
  match x with
  | Error e -> Error e
  | Ok x -> Ok (f x);;

Parse.parse "42.42" (module String3) |> String3.to_string;;

Parse.parse "(42.42)" (module String3) |> String3.to_string;;

Parse.parse "((42.42))" (module String3) |> String3.to_string;;

Parse.parse "( 42.42 )" (module String3) |> String3.to_string;;

Parse.parse " (42.42) " (module String3) |> String3.to_string;;

Parse.parse "1." (module String3) |> String3.to_string;;

Parse.parse "1.+2." (module String3) |> String3.to_string;;

Parse.parse "1.*2." (module String3) |> String3.to_string;;

Parse.parse "1. + 2." (module String3) |> String3.to_string;;

Parse.parse "1. * 2." (module String3) |> String3.to_string;;
Parse.parse "1. + 2. * 3." (module String3) |> String3.to_string;;
Parse.parse "(1. + 2.) * 3." (module String3) |> String3.to_string;;
Parse.parse "(1. + 2.) * (3.  * 4. * (1.+ 2. )) " (module String3) |> String3.to_string;;
