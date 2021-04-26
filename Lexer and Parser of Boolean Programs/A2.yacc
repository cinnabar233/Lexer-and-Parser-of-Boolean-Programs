(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name A2

%term
  CONST of string |NOT  | AND | OR  | XOR   | EQUALS | IMPLIES  | ID of string | RPAREN  | LPAREN  | EOF  | TERM  | IF | THEN| ELSE | PROGRAM | FORMULA | STATEMENT | STATEMENTS


%nonterm   start of string | program of string   | formula of string | statement of string 
%pos int

(*optional declarations *)
 
%eop EOF 
%noshift EOF 

(* %header  *)

%right IF THEN ELSE
%right IMPLIES 
%left AND OR XOR EQUALS
%right NOT

%start start

%verbose

%%
start : program (program ^ "start -> program")

program : statement program (statement1 ^ program1 ^ "program -> statement program,")
          |statement (statement1 ^  "program -> statement,")

statement : formula TERM (formula1 ^  "TERM \";\"," ^  "statement -> formula TERM,")

formula : formula AND formula(formula1 ^  "AND \"AND\"," ^ formula2 ^ "formula -> formula AND formula," )
         | formula OR formula (formula1 ^ "OR \"OR\"," ^  formula2 ^ "formula -> formula OR formula,")
         | formula XOR formula (formula1 ^ "XOR \"XOR\"," ^ formula2 ^ "formula -> formula XOR formula," )
         | formula EQUALS formula (formula1 ^ "EQUALS \"EQUALS\"," ^ formula2 ^ "formula -> formula EQUALS formula," )
        | formula IMPLIES formula (formula1 ^ "IMPLIES \"IMPLIES\"," ^ formula2 ^ "formula -> formula IMPLIES formula,"  )
        | NOT formula ("NOT \"NOT\"," ^ formula1 ^ "formula -> NOT formula,")
        | LPAREN formula RPAREN ("LPAREN \"(\","  ^ formula1 ^ "RPAREN \")\"," ^ "formula -> LPAREN formula RPAREN,"  )
        | ID("ID \"" ^ ID ^ "\"," ^"formula -> ID,")
        | CONST("CONST \"" ^ CONST ^ "\"," ^"formula -> CONST,")
        | IF formula THEN formula ELSE formula ("IF \"IF\"," ^ formula1 ^ "THEN \"THEN\","^ formula2 ^ "ELSE \"ELSE\"," ^ formula3 ^ "formula -> IF formula THEN formula ELSE formula,")



         

