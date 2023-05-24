//module Lambda7c
open System;
open Recless.Base
open Option
//open Lambda7c

//// fsharpc llschemer.fs -r recless.dll -r schemer_lex.dll

// semantic value type for scheme
type expr =
  | Integer of int
  | Floatpt of float
  | Strlit of string
  | Var of string
  | TypedVar of lltype*string
  | Nil
  | Binop of string*expr*expr
  | Uniop of string*expr
  | Ifelse of expr*expr*expr
  | Whileloop of expr*expr
  | Define of LBox<string>*expr
  | TypedDefine of LBox<lltype*string>*expr
  | Lambda of (string list)*expr
  | TypedLambda of ((lltype*string) list)*lltype*expr
  | Let of LBox<string>*expr*LBox<expr>
  | TypedLet of LBox<lltype*string>*expr*LBox<expr>
  | Quote of expr               // may not need for now
  | Setq of LBox<string>*expr   // destructive assignment (lvalue can be vector)
  | Sequence of LBox<expr> list  // includes function application (f x y z)
  // may want to change above for a more standard case for function application:
  // | Apply of expr*(expr list)
  | Beginseq of LBox<expr> list  // sequence of expressions
  // for vectors: (added later)
  | Vector of LBox<expr> list    // [1 2 3]
  | Vector_make of LBox<expr>*expr  //  (vmake 0 4)
  | Vector_setq of LBox<expr>*expr*expr   //  (vsetq a 0 8) 
  | Vector_get of LBox<expr>*expr        //  (vget a 1)
  // type expressions  
  | TypeExpr of lltype
  | Typedval of (lltype*expr)
  | Label of string   // not a proper expression - just a temporary
  | Error
  //  | Continuation of (expr -> expr)    // don't need

and lltype =
  | LLint | LLfloat | LLstring
  | LList of lltype | LLtuple of lltype list
  | LLfun of (lltype list)*lltype
  | LLunknown | LLuntypable | LLvar of string | LLunit


let rec type_to_str = function // pretty-print type expressions
  | LLint -> "int"
  | LLfloat -> "float"
  | LLstring -> "string"
  | LLunit -> "unit"
  | LLvar(x) -> x
  | LLunknown -> "?"
  | LLuntypable -> "type error"
  | LList(t) -> "["+(type_to_str t)+"]"
  | LLtuple(ts) ->
    let mutable s = "("
    for t in ts do s <- s+(type_to_str t)+","
    s+")"
  | LLfun([LLfun(_,_) as ta],tb)-> "("+(type_to_str ta)+") -> "+(type_to_str tb)
  | LLfun([ta],tb) -> (type_to_str ta)+" -> "+(type_to_str tb)
  | LLfun(args,rt) ->
    (type_to_str (LLtuple args)) + " -> " + (type_to_str rt)


// grammar and semantic actions:
// topsym is Pxprplus
let mutable Gmr = new_grammar<expr>("Pxprplus")
Gmr.terminals(["+";"-";"*";"/";"%";"(";")";"^";"=";"<";">";"<=";">=";"[";"]"])
Gmr.terminals(["if";"define";"cons";"car";"cdr";"nil";"setq";"lambda";"let"])
Gmr.terminals(["neq";"eq";"and";"or";"not";"display";"while";"begin";"of"])
Gmr.terminals(["vsetq";"vmake";"vget"])
Gmr.valueterminal("integer","Num",fun n->expr.Integer(int(n)))
Gmr.valueterminal("floatpt","Float",fun n->expr.Floatpt(float(n)))
Gmr.valueterminal("strlit","StrLit",fun n->expr.Strlit(n))
Gmr.valueterminal("var","Alphanum",fun n -> expr.Var(n))
Gmr.lexterminal("NEG","~")
Gmr.lexterminal("QUOTE","'")
Gmr.lexterminal("DOT",".")
Gmr.lexterminal("COMMA",",")
Gmr.lexterminal("COLON",":")
Gmr.lexterminal("ARROW","->")
Gmr.lexterminal("DOLLAR","$")
Gmr.terminals(["int";"string";"float"])
Gmr.nonterminals(["Pxpr";"Expr";"Pxpropt";"varstar";"Binop";"Uniop"])
Gmr.nonterminals(["Txpr";"Typeopt";"Vartyopt";"Typelambdaopt"])

// rules and actions

// Pxpr --> integer | floatpt | strlit | var(:type)?
let semact1 (rhs:Vec<Stackitem<expr>>) = rhs.[0].value
Gmr.production("Pxpr --> integer",semact1);
Gmr.production("Pxpr --> strlit",semact1);
Gmr.production("Pxpr --> floatpt",semact1);

let semact1b (rhs:Vec<Stackitem<expr>>) =
  match (rhs.[0].value, rhs.[1].value) with
    | (Var(x), TypeExpr(LLunknown)) -> rhs.[0].value
    | (Var(x), TypeExpr(typ)) -> TypedVar(typ,x)
    | _ -> Error
Gmr.production("Vartyopt --> var Typeopt",semact1b);    
Gmr.production("Pxpr --> Vartyopt",semact1);
Gmr.production("Typeopt --> ", fun r -> TypeExpr(LLunknown))
Gmr.production("Typeopt --> COLON Txpr",fun r->r.[1].value)

// Pxpr --> nil
let semact5 (rhs:Vec<Stackitem<expr>>) = expr.Nil
Gmr.production("Pxpr --> nil",semact5)

// Pxpr --> ( Expr )
let semact6 (rhs:Vec<Stackitem<expr>>) = rhs.[1].value
Gmr.production("Pxpr --> ( Expr )",semact6)

// Expr --> Binop Pxpr Pxpr
let semact7 (rhs:Vec<Stackitem<expr>>) =
  match (rhs.[0].value, rhs.[1].value, rhs.[2].value) with
    | (Label(op), a, b) -> expr.Binop(op,a,b)
    | _ -> Error
Gmr.production("Expr --> Binop Pxpr Pxpr",semact7)

// Expr --> Uniop Pxpr   
let semact8 (rhs:Vec<Stackitem<expr>>) =
  match (rhs.[0].value, rhs.[1].value) with
    | (Label(op), a) -> expr.Uniop(op,a)
    | _ -> Error
Gmr.production("Expr --> Uniop Pxpr",semact8)

// Expr --> if Pxpr Pxpr Pxpr
let semact9 (rhs:Vec<Stackitem<expr>>) =
   Ifelse(rhs.[1].value, rhs.[2].value, rhs.[3].value)
Gmr.production("Expr --> if Pxpr Pxpr Pxpr",semact9)

// Expr --> while Pxpr Pxpr
Gmr.production("Expr --> while Pxpr Pxpr",fun rh -> Whileloop(rh.[1].value,rh.[2].value))

// Expr --> define var Pxpr
let semact10 (rhs:Vec<Stackitem<expr>>) =
  match (rhs.[1].value, rhs.[2].value) with 
    | (Var(s), e) -> Define(rhs.[1].tolbox(s),e)
    | (TypedVar(ty,s),e) -> TypedDefine(rhs.[1].tolbox((ty,s)),e)
    | _ -> Error
Gmr.production("Expr --> define Vartyopt Pxpr",semact10)


// Typelambdaopt --> Pxpr
Gmr.production("Typelambdaopt --> Pxpr",fun r->r.[0].value)
// Typelambdaopt --> COLON Txpr Pxpr
Gmr.production("Typelambdaopt --> COLON Txpr Pxpr",fun r-> match r.[1].value with | TypeExpr(ty) -> Typedval(ty,r.[2].value) | _ -> Error)

// make everything a typed lambda

// Expr --> lambda ( varstar ) Typelambdaopt
let semact11 (rhs:Vec<Stackitem<expr>>) =
   match (rhs.[2].value, rhs.[4].value) with
     | (Sequence(vs),Typedval(rt,p)) ->
       let mutable Typedlist = []
       for s in vs do
         match s.value with
           | TypedVar(tx,x) -> Typedlist<-(tx,x)::Typedlist
           | Var(x) -> Typedlist<-(LLunknown,x)::Typedlist
           | _ -> ()
       TypedLambda(Typedlist,rt,p)
     | (Sequence(vs),p) ->    // no return type specified
       let mutable Typedlist = []
       for s in vs do
         match s.value with
           | TypedVar(tx,x) -> Typedlist<-(tx,x)::Typedlist
           | Var(x) -> Typedlist<-(LLunknown,x)::Typedlist
           | _ -> ()
       TypedLambda(Typedlist,LLunknown,p)
     | _ -> Error
Gmr.production("Expr --> lambda ( varstar ) Typelambdaopt",semact11)

// Expr --> QUOTE Pxpr
let semact12 (rhs:Vec<Stackitem<expr>>) = Quote(rhs.[1].value)
Gmr.production("Expr --> QUOTE Pxpr",semact12)

// Expr --> setq var Pxpr
let semact13 (rhs:Vec<Stackitem<expr>>) =
  match (rhs.[1].value, rhs.[2].value) with
    | (Var(x), p) -> Setq(rhs.[1].tolbox(x),p)
    //    | (Index(aexpr,iexpr),p) -> Setq(rhs.[1],p)
    | _ -> Error
Gmr.production("Expr --> setq var Pxpr",semact13)

// Expr --> Pxprplus
Gmr.production("Expr --> Pxprplus",semact1)

let semact14 (rhs:Vec<Stackitem<expr>>) =
  match (rhs.[2].value, rhs.[3].value, rhs.[5]) with
    | (Var(x),v,b) -> Let(rhs.[2].tolbox(x),v,b)
    | (TypedVar(tx,x),v,b) -> TypedLet(rhs.[2].tolbox((tx,x)),v,b)
    | _ -> Error
Gmr.production("Expr --> let ( Vartyopt Pxpr ) Pxpr",semact14)


//Pxpropt -->
let semact15 (rhs:Vec<Stackitem<expr>>) = Sequence([])
Gmr.production("Pxpropt -->",semact15)

//Pxpropt -->  Pxprplus
let semact16 (rhs:Vec<Stackitem<expr>>) = rhs.[0].value
Gmr.production("Pxpropt --> Pxprplus",semact16)

//Pxprplus --> Pxpr Pxpropt 
let semact17 (rhs:Vec<Stackitem<expr>>) = 
   match rhs.[1].value with
     | Sequence(v) ->  Sequence(rhs.[0]::v)
     | _ -> Error
Gmr.production("Pxprplus --> Pxpr Pxpropt",semact17)

let semact18 (rhs:Vec<Stackitem<expr>>) =
   match rhs.[1].value with
     | Sequence(s) -> Beginseq(s)
     | _ -> Error
Gmr.production("Expr --> begin Pxprplus",semact18)

// Binop --> +, Uniop --> car, etc
let semact20 sym (rhs:Vec<Stackitem<expr>>) = Label(sym)  //curried function
Gmr.production("Binop --> +",semact20 "+")
Gmr.production("Binop --> *",semact20 "*")
Gmr.production("Binop --> -",semact20 "-")
Gmr.production("Binop --> /",semact20 "/")
Gmr.production("Binop --> %",semact20 "%")
Gmr.production("Binop --> =",semact20 "=")
Gmr.production("Binop --> <",semact20 "<")
Gmr.production("Binop --> >",semact20 ">")
Gmr.production("Binop --> <=",semact20 "<=")
Gmr.production("Binop --> >=",semact20 ">=")
Gmr.production("Binop --> cons",semact20 "cons")
Gmr.production("Binop --> eq",semact20 "eq")
Gmr.production("Binop --> and",semact20 "and")
Gmr.production("Binop --> or",semact20 "or")
Gmr.production("Uniop --> not",semact20 "not")
Gmr.production("Uniop --> NEG",semact20 "~")
Gmr.production("Uniop --> car",semact20 "car")
Gmr.production("Uniop --> cdr",semact20 "cdr")
Gmr.production("Uniop --> display",semact20 "display")

//varstar -->
let semact28 (rhs:Vec<Stackitem<expr>>) = Sequence([])
Gmr.production("varstar -->",semact28)

//varstar --> Vartyopt varstar
let semact29 (rhs:Vec<Stackitem<expr>>) =
   match rhs.[1].value with
     | Sequence(v) ->  Sequence(rhs.[0]::v)
     | _ -> Error
Gmr.production("varstar --> Vartyopt varstar",semact29)

// types
Gmr.production("Txpr --> int",fun r -> TypeExpr(LLint))
Gmr.production("Txpr --> float",fun r -> TypeExpr(LLfloat))
Gmr.production("Txpr --> string",fun r -> TypeExpr(LLstring))
// more needed later

///////////////// For vectors
Gmr.production("Txpr --> [ Txpr ]", fun rhs ->
  match rhs.[1].value with
    | (TypeExpr(t)) -> TypeExpr(LList(t))
    | _ -> Error
)
Gmr.production("Pxpr --> [ Pxpropt ]", fun rhs ->
  match rhs.[1].value with
    | Sequence(s) -> Vector(s)
    | _ -> Error
)    
Gmr.production("Expr --> vget Pxpr Pxpr",
  fun rhs -> Vector_get(rhs.[1],rhs.[2].value))
Gmr.production("Expr --> vsetq Pxpr Pxpr Pxpr",
  fun rhs -> Vector_setq(rhs.[1],rhs.[2].value,rhs.[3].value))
Gmr.production("Expr --> vmake Pxpr Pxpr",
  fun rhs -> Vector_make(rhs.[1],rhs.[2].value))  


//////////// ll1 parser
let mutable GENERATE = false
let argv = Environment.GetCommandLineArgs();
if argv.Length>1 && argv.[1]="-generate" then GENERATE <- true
if argv.Length>2 && argv.[2]="-trace" then TRACE <- true

let mutable parser1 = Unchecked.defaultof<LLparser<expr>>

if GENERATE then
  parser1 <- make_parser(Gmr,null)
  parser1.to_json("llschemer")
  if TRACE then (Gmr.printgrammar(); printfn "parser saved")
elif not(GENERATE) then
  parser1 <- load_parser_into(Gmr,"llschemer.json",null,false)
  if TRACE then
    parser1.Gmr.printgrammar()
    printfn "parser loaded"


let mutable source = ""
if argv.Length > 1 && argv.[1] <> "-generate" then source <- argv.[1]


//////// testing
if source="" then
  printf "Enter Expression: "
  let lexer1 = Fussless.schemerlexer(Console.ReadLine());
  parser1.set_scanner lexer1
else
  let fd = new System.IO.FileStream(source ,System.IO.FileMode.Open);
  let lexer1 = Fussless.schemerlexer(fd)
  parser1.set_scanner lexer1

let result_ast = parser1.parse()
if not(parser1.errors) then printfn "result = %A" result_ast


// compile with recless.dll, schemer_lex.dll

///////////////////////////////// metatype for llschemer types

(*
type lltype = LLint | LLfloat | LLstring | LList of lltype | LLtuple of lltype list | LLfun of (lltype list)*lltype | LLunknown | LLuntypable | LLvar of string
// unkown means yet to be determined, untypable means error in type inference.
*)



// tests if a type is grounded (no variables)
let rec grounded_type = function    // no unknowns
  | LLunknown | LLvar(_) | LLuntypable -> false
  | LList(t) -> grounded_type t
  | lltype.LLtuple(vs) -> List.forall grounded_type vs
  | LLfun(args,rtype) ->  List.forall grounded_type (rtype::args)
  | _ -> true


// symbol table
let mutable global_index = 0; // to distinguish vars in different scopes.
type table_entry =
  {
     mutable typeof:lltype;
     gindex:int;  // unqiue index for brute-force alpha-conversion
     ast_rep : expr option;  // link to ast representation (just pointer)
  }
and table_frame =
  {
    name : string;
    entries:HashMap<string, table_entry>;
    parent_scope:table_frame option;
  }
  member this.add_entry(s:string,t:lltype,a:expr option) = //overwrite
    global_index <- global_index + 1
    this.entries.Add(s,{typeof=t; gindex=global_index; ast_rep=a;})
  member this.get_entry(s:string) =
    let mutable current = this
    let mutable hasentry = true
    while hasentry && not(current.entries.ContainsKey(s)) do
      if (isSome current.parent_scope) then
        current <- current.parent_scope.Value
      else hasentry <- false
    if hasentry then (Some current.entries.[s])
    else None
  member this.get_type(s:string) =
    let mutable current = this
    let mutable hasentry = true
    while hasentry && not(current.entries.ContainsKey(s)) do
      if (isSome current.parent_scope) then
        current <- current.parent_scope.Value
      else hasentry <- false
    if hasentry then current.entries.[s].typeof
    else LLuntypable
  member this.set_type(s:string, t:lltype) =
    let mutable current = this
    let mutable hasentry = true
    while hasentry && not(current.entries.ContainsKey(s)) do
      if (isSome current.parent_scope) then
        current <- current.parent_scope.Value
      else hasentry <- false
    if hasentry then
      let current_type =  current.entries.[s].typeof
      if current_type=LLunknown || current_type=t then
         current.entries.[s].typeof <- t
         true
      else false
    else false
// end of impl table_frame    

// an expression type checks if and only if a type can be derived for it.
// what is type of empty vector? must have a category of syntactic expressions
// called type.  unless we have logic vars and unification, can't really
// do type inference.

// non-polymorphic

// must allow grammar to express types.
// Txpr --> int_t | float_t | string_t | 
// OK: try to get away without syntactic type expressions. with unkown ast
// type.

let rec resolve ta tb =
  match (ta,tb) with
    | (_,_) when ta=tb -> true
    | (LLunknown,tb) when tb<>LLuntypable -> true
    | (ta,LLunknown) when ta<>LLuntypable -> true
    | (LList(a), LList(b)) -> resolve a b
    | (LLtuple(av),LLtuple(bv)) when av.Length=bv.Length ->
       List.forall (fun x->x) [for i in 0..av.Length-1 -> resolve (av.[i]) (bv.[i])]
    | (LLfun(av,r), LLfun(bv,q)) -> resolve (LLtuple(r::av)) (LLtuple(q::bv))
    | _ -> false

// type inference - expected=unknown means infer type, else check type
let rec infer_type (symtable:table_frame) expected (expression:expr) =
  match (expression,expected) with
    | (Integer(_),LLunknown) | (Integer(_),LLint) -> lltype.LLint
    | (Floatpt(_),LLunknown) | (Floatpt(_),LLfloat) -> LLfloat
    | (Strlit(_),LLunknown) | (Strlit(_),LLstring) -> LLstring
    | (Var(x),LLunknown) -> symtable.get_type(x)
    (*
        let knowntype =  
        if knowntype=LLuntypable then symtable.set_type(x,LLunknown) |> ignore
        knowntype
    *)
    | (Var(x), _) when grounded_type expected ->  // set type entry
        if expected<>LLuntypable && symtable.set_type(x,expected) then expected
        else LLuntypable
    | (Nil,_) when grounded_type expected -> LList(expected)
    | (TypedDefine(Lbox((tx,x)) as lb,e),LLunknown) ->
         let etype = infer_type symtable tx e
         if etype=tx then
           symtable.add_entry(x,etype,Some e) |> ignore
           etype
         else
           printfn "TYPE ERROR for definintion on line %d, column %d" (1+lb.line) lb.column
           LLuntypable
    | (Define(Lbox(x) as lb,e),LLunknown) ->
         let etype = infer_type symtable LLunknown e
         if etype<>LLuntypable then
           symtable.add_entry(x,etype,Some e) |> ignore
           printfn "(def) Type inferred for %s = %A" x etype
           etype
         else
           printfn "TYPE ERROR for definintion of (%A) on line %d, column %d" e (1+lb.line) lb.column
           LLuntypable
    | (Ifelse(a,b,c), LLunknown) ->  
         let atype = infer_type symtable LLint a
         let btype = infer_type symtable LLunknown b
         let ctype = infer_type symtable LLunknown c
         if atype=LLint && btype<>LLuntypable && ctype=btype then btype
         else LLuntypable
    | (Ifelse(a,b,c), etype) ->
         let atype = infer_type symtable LLint a
         let btype = infer_type symtable etype b
         let ctype = infer_type symtable etype c
         if atype=LLint && btype=etype && ctype=etype then etype
         else LLuntypable
    | (Whileloop(a,b), LLunknown) ->
         let atype = infer_type symtable LLint a
         let btype = infer_type symtable LLunknown b
         if atype=LLint then btype else LLuntypable
    | (Whileloop(a,b), etype) ->
         let atype = infer_type symtable LLint a
         let btype = infer_type symtable etype b
         if atype=LLint && btype=etype then etype else LLuntypable
    | (TypedLambda(formal_args,rtype,body), _) ->
       // create new table frame
       let local_frame = { table_frame.name="lambda"; entries=HashMap<string,table_entry>(); parent_scope = Some symtable; }
       for (ta,a) in formal_args do local_frame.add_entry(a,ta,None)
       let btype = infer_type local_frame rtype body
       // now look at inferred types
       let inferred_args_t = [for (ta,a) in formal_args -> local_frame.get_type(a)]
       if (resolve btype rtype) then LLfun(inferred_args_t,btype)
       else LLuntypable
    | (Lambda(formal_args,body), LLunknown) ->
       // create new table frame
       let local_frame = { table_frame.name="lambda"; entries=HashMap<string,table_entry>(); parent_scope = Some symtable; }
       for a in formal_args do local_frame.add_entry(a,LLunknown,None)
       let btype = infer_type local_frame LLunknown body
       // now look at inferred types
       let inferred_args_t = [for a in formal_args -> local_frame.get_type(a)]
       if btype<>LLuntypable then LLfun(inferred_args_t,btype)
       else LLuntypable       
    | (TypedLet(Lbox((tx,x)) as lb,v,Lbox(e)),_) ->
       let vtype = infer_type symtable tx v
       if vtype<>tx then
         printfn "Type error in Let-expression line %d" (lb.line+1)
         LLuntypable
       else
         let local_frame = { table_frame.name="let_"+x; entries=HashMap<string,table_entry>(); parent_scope = Some symtable; }
         local_frame.add_entry(x,vtype,Some(v))
         let rtype = infer_type local_frame expected e
         if expected<>LLunknown && rtype<>expected then LLuntypable
         else rtype
    | (Let(Lbox(x),v,Lbox(e)),_) ->
       let vtype = infer_type symtable LLunknown v
       if vtype=LLuntypable || not(grounded_type vtype) then
         printfn "Type error in Let-expression"
         LLuntypable
       else
         let local_frame = { table_frame.name="let_"+x; entries=HashMap<string,table_entry>(); parent_scope = Some symtable; }
         local_frame.add_entry(x,vtype,Some(v))
         let rtype = infer_type local_frame expected e
         if expected<>LLunknown && rtype<>expected then LLuntypable
         else rtype
    | (Setq(Lbox(x) as lv,rv), LLunknown) ->
      let ltype = symtable.get_type(x)
      if ltype=LLuntypable then
        printfn "unbound identifier %s in assignment, line %d column %d" x lv.line lv.column
        ltype
      else 
        let rtype = infer_type symtable ltype rv
        printfn "setq ltype %A rtype %A" ltype rtype
        if (resolve rtype ltype) then rtype else LLuntypable
    | (Sequence(f::rest), LLunknown) ->  // function call case
      let ftype = infer_type symtable LLunknown f.value
      match ftype with
        | LLfun(fargs,rettype) when rest.Length=fargs.Length ->
           let argtypes = [for r in 0..rest.Length-1 -> infer_type symtable fargs.[r] rest.[r].value ]
           if fargs=argtypes then rettype 
           else
             printfn "actual args passed to %A don't match expected types" f.value
             LLuntypable
        | _ -> LLuntypable
    | (Sequence(f::rest), _) when grounded_type expected ->
      let ftype = infer_type symtable LLunknown f.value
      let argtypes = [ for r in rest -> infer_type symtable LLunknown r.value ]
      match ftype with
        | LLfun(fargs,rtype) when fargs=argtypes && rtype=expected -> rtype
        | _ -> LLuntypable
    | (Beginseq([Lbox(a)]), etype) -> infer_type symtable etype a
    | (Beginseq(a::rest), etype) ->
      let atype = infer_type symtable LLunknown a.value
      if atype<>LLuntypable then infer_type symtable etype (Beginseq rest)
      else LLuntypable
    | (Binop("cons",a,b), LLunknown) ->
        let atype = infer_type symtable LLunknown a
        let btype = infer_type symtable LLunknown b
        match (atype,btype) with
          | (t1, LList(t2)) when t1=t2 -> LList(t2)
          | (LLunknown, LList(t2)) -> LList(t2)
          | (t1, LLunknown) -> LList(t1)
          | _ -> LLuntypable
    | (Binop("cons",a,b), LList(ctype)) ->
        let atype = infer_type symtable LLunknown a
        let btype = infer_type symtable LLunknown b
        if atype=ctype && btype=LList(ctype) then LList(ctype)
        else LLuntypable
    | (Uniop("car",m), LLunknown) ->
        let mtype = infer_type symtable LLunknown m
        match mtype with
          | LList(atype) -> atype
          | _ -> LLuntypable
    | (Uniop("car",m), _) ->  // known type
        let mtype = infer_type symtable (LList expected) m
        match mtype with
          | LList(atype) -> atype
          | _ -> LLuntypable
    | (Uniop("cdr",m), LLunknown) ->
        let mtype = infer_type symtable LLunknown m
        match mtype with
          | LList(atype) -> mtype
          | _ -> LLuntypable
    | (Uniop("cdr",m), _) ->  // known type
        let mtype = infer_type symtable expected m
        if mtype=expected then expected
        else LLuntypable
    | (Uniop("display",m),_) -> infer_type symtable expected m
    | (Binop(op,a,b),LLunknown) ->
         let atype = infer_type symtable LLunknown a
         let btype = infer_type symtable LLunknown b
         match (atype,btype) with
           | (LLint,LLint) | (LLfloat,LLfloat) -> atype
           | (LLunknown,LLunknown) ->  // assume default type int
             let atype2 = infer_type symtable LLint a
             let btype2 = infer_type symtable LLint b
             if atype2=LLint && btype2=LLint then LLint else LLuntypable
           | (LLint,LLunknown) ->
               let btype2 = infer_type symtable LLint b
               if btype2=atype then atype else LLuntypable
           | (LLunknown,LLint) ->
               let atype2 = infer_type symtable LLint a
               if atype2=btype then btype else LLuntypable
           | (LLfloat,LLunknown) ->
               let btype2=infer_type symtable LLfloat b
               if btype2=atype then atype else LLuntypable
           | (LLunknown,LLfloat) ->
               let atype2 = infer_type symtable LLfloat a
               //printfn "type for a: %A, type for b: %A" atype2 btype
               if atype2=btype then btype else LLuntypable
           | _ -> LLuntypable
    | (Binop(op,a,b),LLint) ->
         let atype = infer_type symtable LLint a
         let btype = infer_type symtable LLint b
         if atype=LLint && btype=LLint then LLint
         else LLuntypable
    | (Binop(op,a,b), LLfloat) ->
         let atype = infer_type symtable LLfloat a
         let btype = infer_type symtable LLfloat b
         if atype=LLfloat && atype=btype then LLfloat
         else LLuntypable
    | _ -> LLuntypable

// trying to cheat and avoid a full-scale matching/unification algorithm
// must be just LLunknown or grounded type or LLuntypable.
// force inference to return a grounded type.

// global symbol table
let mutable symbol_table =
  {
    table_frame.name = "global";
    entries= HashMap<string,table_entry>();
    parent_scope = None;
  }

type SymbolTable = 
  {
     mutable current_frame: table_frame;
     mutable gindex: int;
     frame_hash:HashMap<(int*int),table_frame>;
  }
  member this.add_entry(s:string,t:lltype,a:expr option) = //overwrite
    this.gindex <- this.gindex + 1
    this.current_frame.entries.Add(s,{typeof=t; gindex=this.gindex; ast_rep=a;})  
  member this.frame_add_entry(s:string,t:lltype,a:expr option) = //overwrite
    this.gindex <- this.gindex + 1
    this.current_frame.entries.Add(s,{typeof=t; gindex=this.gindex; ast_rep=a;})
    
  member this.push_frame(n,line,column) =
    let newframe =
      {
        table_frame.name=n;
        entries=HashMap<string,table_entry>();
        parent_scope = Some(this.current_frame);
      }
    this.frame_hash.[(line,column)] <- newframe
    this.current_frame <- newframe
  member this.pop_frame() = // don't call on global frame
    this.current_frame.parent_scope |> map (fun p -> this.current_frame<-p)
  member this.get_entry(s:string) = this.current_frame.get_entry(s)
  member this.newvar prefix =
    this.gindex <- this.gindex + 1
    sprintf "%s_%d" prefix this.gindex
//// global symbol table impl
// global symbol table
let mutable global_frame =
  {
    table_frame.name = "global";
    entries= HashMap<string,table_entry>();
    parent_scope = None;
  }
let symbol_table2 =
  {
    SymbolTable.current_frame=global_frame;
    gindex = 0;
    frame_hash = HashMap<(int*int),table_frame>();
  }

// alternative table_entry union:

type TableEntry =
  | Simple of typeof:lltype * gindex:int * ast_rep:option<expr>
  | LambdaDef of typeof:lltype *gindex:int * frame:table_frame * ast_rep:option<expr>
  

////////////////////////////////
let mutable TYPE_ERRORS = false; // global var! (who cares)

let rec type_check (exprs:LBox<expr> list) =
  match exprs with
    | (lb::tail) ->
      let vtype = infer_type symbol_table LLunknown lb.value
      if vtype=LLuntypable then
        printfn "Error type checking expression at line %d column %d" (lb.line+1) lb.column
        false
      elif not(grounded_type vtype) then
        printfn "TYPE ERROR: ungrounded type %A, line %d column %d" vtype (lb.line+1) lb.column
        false
      else
        printfn "Type inferred for line %d: %A" (lb.line+1) vtype
        type_check tail
    | [] -> true

if not(parser1.errors) then
  match result_ast with
    | Sequence(exprs) ->
      TYPE_ERRORS <- not(type_check exprs)
    | _ -> ()
else printfn "no type checking on result"    

if not(TYPE_ERRORS) then printfn "Type checking successful"

printfn "Symbol Table: %A" symbol_table



//////////////////////////////////////////////////
//////////////////unification
// returns true on success (x does NOT appear in ty)
let rec occurs_check (x:string) (tyxpr:lltype) =
  match tyxpr with
    | LLvar(y) when x=y -> false
    | LList(ty) -> occurs_check x ty
    | LLtuple(tylist) -> List.forall (fun y->occurs_check x y) tylist
    | LLfun(domain,codomain) ->
        List.forall (fun y->occurs_check x y) (codomain::domain)
    | _ -> true

// substitute LLvar(x) with t in type expression txpr
let rec apply_subst (x:string) (t:lltype) (txpr:lltype) =
  match txpr with
    | LLvar(y) when x=y -> t
    | LList(ty) -> LList(apply_subst x t ty)
    | LLtuple(tys) ->
       let tysubst = [ for ty in tys -> apply_subst x t ty ]
       LLtuple(tysubst)
    | LLfun(domain,codomain) ->
       let sdomain = [ for ty in domain -> apply_subst x t ty ]
       let scodomain = apply_subst x t codomain
       LLfun(sdomain,scodomain)
    | _ -> txpr   // unchanged

type Equations = Vec<lltype*lltype>

// function must be passed a pointer to a hashmap and set of equations
// function terminates when equations is empty
let unify (equations:Equations) =
  let mutable unifier = HashMap<string,lltype>()
  let mutable failure = false
  let mutable index = 0
  while index < equations.Count && not(failure) do
    let (t1,t2) = equations.[index]
    match (t1,t2) with
      | (_,_) when t1=t2 -> ()
      | (LLvar x,t3) | (t3,LLvar x) when occurs_check x t3 ->
        for i in 0 .. equations.Count-1 do
          if index<>i then
            let (ta,tb) = equations.[i]
            let ta2 = apply_subst x t3 ta
            let tb2 = apply_subst x t3 tb          
            equations.[i] <- (ta2,tb2)
      | (LLint,LLint) | (LLfloat,LLfloat) | (LLstring,LLstring) -> ()
      | (LLunit,LLunit) -> ()
      | (LList(ta),LList(tb)) -> equations.Add(ta,tb)
      | (LLtuple(tav),LLtuple(tbv)) when tav.Length=tbv.Length ->
          for i in 0..tav.Length-1 do equations.Add(tav.[i],tbv.[i])
      | (LLfun(d1,cd1),LLfun(d2,cd2)) when d1.Length=d2.Length ->
          for i in 0..d1.Length-1 do equations.Add(d1.[i],d2.[i])
          equations.Add(cd1,cd2)
      | _ -> failure <- true
    index <- index + 1
  while index>0 && not(failure) do
    index <- index - 1
    match equations.[index] with
      | (LLvar x, t3) | (t3,LLvar x) when not(unifier.ContainsKey(x)) ->
         unifier.[x] <- t3
      | _ -> ()
  // second while
  if failure then None else Some(unifier)
////////////// unification


//// simple AST for pure lambda terms

type lambdaterm = Termvar of string | Abs of string*lambdaterm | App of lambdaterm*lambdaterm

// use same type system, target is target type variable

let newtable() = 
  let mutable root_frame =
    {
      table_frame.name = "root";
      entries= HashMap<string,table_entry>();
      parent_scope = None;
    }
  let symtable =
    {
      SymbolTable.current_frame=root_frame;
      gindex = 0;
      frame_hash = HashMap<(int*int),table_frame>();
    }
  symtable

// using global symbol table and system of equations
let mutable symtable = newtable()
let mutable equations = Equations()

let rec infer_setup (term:lambdaterm) =
  match term with
    | Abs(x,m) -> // lambda x.m
      symtable.push_frame(x,0,0) // not keeping track of line/col in example
      let tx = LLvar(symtable.newvar("t"))
      symtable.add_entry(x,tx,None)  //None=no pointer to ast
      let tm = infer_setup m   // infer type of body
      symtable.pop_frame() |> ignore
      let tabs = LLvar(symtable.newvar("t"))
      equations.Add(tabs,LLfun([tx],tm)) |> ignore
      LLfun([tx],tm)
    | App(a,b) -> //(a b)
      let btype = infer_setup b   
      let atype = infer_setup a
      match atype with
        | LLfun([domain],codomain) ->
          equations.Add((domain,btype)) |> ignore
          codomain
        | LLvar(ta) ->
          let codomain = LLvar(symtable.newvar("t"))
          equations.Add((atype, LLfun([btype],codomain))) |> ignore
          codomain
        |_ -> LLuntypable
    | Termvar(x) ->
      symtable.get_entry(x) |> fold (fun d e -> e.typeof) LLuntypable
      // fold is like map_or: takes default that's returned if option isNone

let infer_type_lambda name term =
  let skeleton_type = infer_setup term
  //printfn "skeleton type: %s" (type_to_str skeleton_type)
  let tvar = symtable.newvar(name)
  equations.Add((LLvar(tvar), skeleton_type))
  let result = unify equations
  match result with
    | Some(unifier) when unifier.ContainsKey(tvar) -> unifier.[tvar]
    | _ -> LLuntypable
      
    
let K = Abs("x",Abs("y",Termvar("x")))  // K = fun x y -> x

let typeK = infer_type_lambda "k" K
printfn "type of K: %s" (type_to_str typeK)

let S = Abs("x",Abs("y",Abs("z",App(App(Termvar "x",Termvar "z"),App(Termvar "y",Termvar "z"))))) // S = fun x y z -> x z (y z)

symtable <- newtable() // fresh symbol table
equations <- Equations()
printfn "type of S: %s" (type_to_str (infer_type_lambda "s" S))


// tests if a unifier is grounded (no free type variables after substitutions)
let grounded_unifier (unifier:HashMap<string,lltype>) =
  let mutable answer = true
  try  // simulate break by throwing exception (down with the establishment!)
    for t in unifier.Values do
      if not(grounded_type t) then raise(Exception "break")
  with | _ -> answer <- false
  answer


Gmr.printgrammar()
