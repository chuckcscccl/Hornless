open System;
open Hornless.Base

//////fsharpc llcalc.fs -r recless.dll -r ll1_lex.dll

//TRACE <- true

// semantic value type
type semval_t = Number of int | Continuation of (int -> int) | Nothing

// semantic actions for rules
let semact1 (rhs:Vec<Stackitem<semval_t>>) =  // E --> T E1
  match (rhs.[0].value, rhs.[1].value) with
    | (Number(x), Continuation(f)) -> Number(f x)
    | _ -> Nothing

let semact2 (rhs:Vec<Stackitem<semval_t>>) =  // E1 --> - T E1
  match (rhs.[1].value, rhs.[2].value) with
    | (Number(x), Continuation(f)) -> Continuation(fun y->f(y-x))
    | _ -> Nothing

let semact3 (rhs:Vec<Stackitem<semval_t>>) =  // E1 --> + T E1
  match (rhs.[1].value, rhs.[2].value) with
    | (Number(x), Continuation(f)) -> Continuation(fun y->f(y+x))
    | _ -> Nothing    

let semact4 (rhs:Vec<Stackitem<semval_t>>) =  // E1 --> epsilon
  Continuation(fun x -> x)

// semact5 for T --> F T1 is same as semact1
let semact5 = semact1

let semact6 (rhs:Vec<Stackitem<semval_t>>) =  // T1 --> * F T1
  match (rhs.[1].value, rhs.[2].value) with
    | (Number(x), Continuation(f)) -> Continuation(fun y->f(y*x))
    | _ -> Nothing

let semact7 (rhs:Vec<Stackitem<semval_t>>) =  // T1 --> DIV F T1
  match (rhs.[1].value, rhs.[2].value) with
    | (Number(x), Continuation(f)) -> Continuation(fun y->f(y/x))
    | _ -> Nothing

(* Do this for a right-associative division operator.  However, 
   this will only work if * is also made right-associative
let semact7r (rhs:Vec<Stackitem<semval_t>>) =  // T1 --> DIV T
  match rhs.[1].value with
    | Number(x) -> Continuation(fun y->(y/x))
    | _ -> Nothing
The rule T1 --> DIV T is derived from the LR grammar T --> F DIV T | F
after left-factoring and the rule T1 --> DIV F T1 is derived from
T --> T DIV F | F  after left-recursion elimination.  Forgive the ugly
grammar: some people like it.  I can only stomach it after I noticed
that the semantic value of T1 can be a continuation function.  Otherwise
you will have to build a "tree" just as ugly as the grammar, something
that will bear no resemblance to the AST you ultimately want, just to
change the associativity of the operator.
*)

let semact8 = semact4  // T1 --> epsilon

let semact9 (rhs:Vec<Stackitem<semval_t>>) =  // F --> num
  rhs.[0].value

let semact10 (rhs:Vec<Stackitem<semval_t>>) =  // F --> ( E )
  rhs.[1].value

let acts = [semact1;semact2;semact3;semact4;semact5;semact6;semact7;semact8;semact9;semact10]


/////////// generate or load

let mutable GENERATE = true
let argv = Environment.GetCommandLineArgs();
if argv.Length>1 then GENERATE <- false

let mutable parser1:LLparser<semval_t> = Unchecked.defaultof<LLparser<semval_t>>

if GENERATE then
  let G = new_grammar<semval_t>("E")
  G.terminals(["+";"-";"*";"(";")"])
  G.valueterminal("num","Num",fun n -> Number(int n))
  G.lexterminal("DIV","/")
  G.nonterminals(["T"; "F"; "E1"; "T1"]);
  G.production("E --> T E1",semact1)  
  G.production("E1 --> - T E1",semact2)
  G.production("E1 --> + T E1", semact3)
  G.production("E1 --> ", semact4)
  G.production("T --> F T1", semact5)
  G.production("T1 --> * F T1",semact6)
  G.production("T1 --> DIV F T1",semact7)
  G.production("T1 -->",semact8)
  G.production("F --> num",semact9)
  G.production("F --> ( E )",semact10)
  parser1 <- generate_parser(G,null);
  parser1.to_json("llcalc");
  make_lexer ("llcalc", G)
  if true||TRACE then
     G.printgrammar()
     printfn "parser saved to llcalc.json"
// GENERATE


if not(GENERATE) then
  parser1 <- load_parser(argv.[1])
  parser1.Gmr.valueterminal("num","Num",fun n -> Number(int n))  
  for i in 1..acts.Length do parser1.Gmr.set_action(i,acts.[i-1])
  if TRACE then printfn "parser loaded"

printf "Enter Expression: "
let lexer1 = Hornless.llcalclexer(Console.ReadLine()); // compile with ll1_lex.dll
parser1.set_scanner lexer1

let result = parser1.parse()
if not(parser1.errors) then printfn "result = %A" result

//////// testing json output
//printfn "JSON: %A" (json_dumps (parser1.to_json("calcll")))

