datatype lexresult= CONST of string |NOT of string | AND of string| OR of string | XOR of string | EQUALS  of string | IMPLIES of string |IF of string |THEN of string |ELSE of string| ID of string| RPAREN of string | LPAREN of string | EOF  | TERM of string 
val linenum = ref 1
val colnum = ref 0 
val flag = ref false
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => if !flag then OS.Process.exit(OS.Process.success) else EOF
fun refinc x  =  (x := !x + 1; !x)
exception UnknownToken of int*int*string ;
%%
  
%structure SimpLex
alpha=[A-Za-z];
ws = [\ \t];

%%
\n => (colnum := 0 ; refinc linenum ; lex() ); 
{ws}+ => (colnum := (!colnum)+size yytext; lex());
";" => (refinc colnum ; TERM ";");
"(" => (refinc colnum ; LPAREN "(");
")" => (refinc colnum ; RPAREN ")");
{alpha}+ => (colnum := !colnum + size(yytext) ; if yytext="IF" then IF "IF" else 
			if yytext="THEN" then THEN "THEN" else 
			if yytext="ELSE" then ELSE "ELSE" else 
			if yytext="IMPLIES" then IMPLIES "IMPLIES" else 
			if yytext="TRUE" then  CONST "TRUE" else 
			if yytext="FALSE" then  CONST "FALSE" else 
			if yytext="AND" then AND "AND" else 
			if yytext="EQUALS" then EQUALS "EQUALS" else 
			if yytext="NOT" then NOT "NOT" else 
			if yytext="OR" then OR "OR" else 
			if yytext="XOR" then XOR "XOR" else  
			ID yytext);
. => (refinc colnum ; (flag := true ) ; error("Unknown Token:"^Int.toString(!linenum)^":"^Int.toString(!colnum)^":"^yytext) ; lex());
