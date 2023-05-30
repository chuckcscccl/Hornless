module Hornless.Json
open System;
open Hornless.Base
open Option
open System.Collections.Generic

//// json parser within Hornless

// semantic value type for json
type jval = Integer of int | Float of float | Bool of bool | Null | Str of string | Map of Dictionary<string,jval> | Seq of jval list | JSONERROR

let rec jsondumps = function   // only for tracing
  | jval.Integer(n) -> string(n)
  | Float(n) -> string(n)
  | Bool(n) -> string(n)
  | Null -> "null"
  | Str(s) -> s  //"\"" + s + "\""
  | Seq(s) ->
     let mutable atleastone = false
     let mutable str = "["
     for v in s do
        if atleastone then str <- str + ", "
        atleastone <- true
        str <- str + (jsondumps v)
     str + "]"
  | Map(m) ->
     let mutable atleastone = false
     let mutable str = "{"
     for kvp in m do
        if atleastone then str <- str + ", "
        atleastone <- true
        str <- str + (sprintf "%s:%s" kvp.Key (jsondumps kvp.Value))
     str + "}"
  | JSONERROR -> raise(Exception("Json processing error"))


// grammar and semantic actions:
// topsym is Pxprplus
let mutable private Gmr = new_grammar<jval>("Value")
Gmr.terminals(["null";"true";"false"])
Gmr.valueterminal("Num","Num",fun n->jval.Integer(int(n)))
Gmr.valueterminal("FLOAT","Float",fun n->jval.Float(float(n)))
Gmr.valueterminal("STRING","StrLit",fun (n:string)-> jval.Str(n.Substring(1,n.Length-2)))
//Gmr.valueterminal("STRING","StrLit",fun n-> jval.Str(n))
Gmr.lexterminal("LBRACE","{")
Gmr.lexterminal("RBRACE","}")
Gmr.lexterminal("LBRACK","[")
Gmr.lexterminal("RBRACK","]")
Gmr.lexterminal("COLON",":")
Gmr.lexterminal("COMMA",",")
Gmr.lexterminal("MINUS","-")
Gmr.nonterminals(["MC";"MS";"LC";"LS";"Number";"Boolean"])

// rules and actions
let passthru0 (rhs:Vec<Stackitem<jval>>) = rhs.[0].value
Gmr.production("Number --> Num",passthru0)
let semact1 (rhs:Vec<LBox<jval>>) =
  match (rhs.[1].value) with
    | Float(n) -> Float(-1.0 * n)
    | Integer(n) -> Integer(-1*n)
    | _ -> JSONERROR
Gmr.production("Number --> MINUS Number",semact1)
Gmr.production("Boolean --> true", (fun rhs -> jval.Bool(true)))
Gmr.production("Boolean --> false", (fun rhs -> jval.Bool(false)))

Gmr.production("Value --> Number", passthru0)
Gmr.production("Value --> Boolean", passthru0)
Gmr.production("Value --> STRING", passthru0)
Gmr.production("Value --> null", (fun r -> jval.Null))

Gmr.production("Value --> LBRACK LC RBRACK",(fun r -> r.[1].value))
Gmr.production("LC --> ", fun r-> jval.Seq([]))
let semactlc (rhs:Vec<LBox<jval>>) =
  match (rhs.[1].value) with
    | jval.Seq(s) -> jval.Seq((rhs.[0].value)::s)
    | _ -> JSONERROR
Gmr.production("LC --> Value LS", semactlc)
Gmr.production("LS --> ", fun r-> jval.Seq([]))
let semactls (rhs:Vec<LBox<jval>>) =
  match (rhs.[2].value) with
    | jval.Seq(s) -> jval.Seq((rhs.[1].value)::s)
    | _ -> JSONERROR
Gmr.production("LS --> COMMA Value LS", semactls)

Gmr.production("Value --> LBRACE MC RBRACE",(fun r -> r.[1].value))
Gmr.production("MC --> ", fun r-> jval.Map(Dictionary<string,jval>()))
let semactmc (rhs:Vec<LBox<jval>>) =
  match (rhs.[0].value, rhs.[3].value) with
    | (jval.Str(s), jval.Map(m)) -> m.[s]<-rhs.[2].value; rhs.[3].value
    | _ -> JSONERROR
Gmr.production("MC --> STRING COLON Value MS",semactmc)
Gmr.production("MS --> ", fun r-> jval.Map(Dictionary<string,jval>()))
let semactms (rhs:Vec<LBox<jval>>) =
  match (rhs.[1].value, rhs.[4].value) with
    | (jval.Str(s), jval.Map(m)) -> m.[s]<-rhs.[3].value; rhs.[4].value
    | _ -> JSONERROR
Gmr.production("MS --> COMMA STRING COLON Value MS",semactms)


//////////// ll1 parser
let mutable GENERATE = false
let argv = Environment.GetCommandLineArgs();
if argv.Length>1 && argv.[1]="-generate" then GENERATE <- true
if argv.Length>2 && argv.[2]="-trace" then TRACE <- true

let mutable parser1 = Unchecked.defaultof<LLparser<jval>>

if GENERATE then
  parser1 <- generate_parser(Gmr,null)
  Gmr.print_table()
  parser1.to_json("jsonparser")
  make_lexer("jsonparser",Gmr)
  if TRACE then (Gmr.printgrammar(); printfn "parser and lexer saved")
else
  //parser1 <- load_parser_into(Gmr,"jsonparser.json",null,false)
  parser1 <- make_skeleton_parser(Gmr)
  let ll1table = [("Value",[("Num",5);("MINUS",5);("true",6);("false",6);("STRING",7);("null",8);("LBRACK",9);("LBRACE",14);]);("METASTART",[("Num",0);("MINUS",0);("true",0);("false",0);("STRING",0);("null",0);("LBRACK",0);("LBRACE",0);]);("MC",[("RBRACE",15);("STRING",16);]);("MS",[("RBRACE",17);("COMMA",18);]);("LC",[("RBRACK",10);("Num",11);("MINUS",11);("true",11);("false",11);("STRING",11);("null",11);("LBRACK",11);("LBRACE",11);]);("LS",[("RBRACK",12);("COMMA",13);]);("Number",[("Num",1);("MINUS",2);]);("Boolean",[("true",3);("false",4);]);]
  for (lhs,row) in ll1table do
    let rowtable = Dictionary<string,int>()
    for (k,v) in row do rowtable.[k] <- v
    Gmr.LL1Table.[lhs] <- rowtable
  if TRACE then
    parser1.Gmr.printgrammar()
    printfn "parser loaded"

//let mutable source = ""
//if argv.Length > 1 && argv.[1] <> "-generate" then source <- argv.[1]

(*
//////// testing
if source="" then
  printf "Enter Expression: "
  let lexer1 = Hornless.jsonparserlexer(Console.ReadLine());
  parser1.set_scanner lexer1
else
  let fd = new System.IO.FileStream(source ,System.IO.FileMode.Open);
  let lexer1 = Hornless.jsonparserlexer(fd)
  parser1.set_scanner lexer1

let result_ast = parser1.parse()
if not(parser1.errors) then printfn "result = %A" result_ast
*)

let parse_json(source:string) =
  let fd = new System.IO.FileStream(source ,System.IO.FileMode.Open);
  let lexer1 = Hornless.jsonparserlexer(fd)
  parser1.set_scanner lexer1
  let res = parser1.parse()
  if not(parser1.errors) && res<>JSONERROR then Some(res) else None

//fsharpc jsonparser.fs -r recless.dll -r jsonparser_lex.dll
