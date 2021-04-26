structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val linenum = ref 1 
  val colnum = ref 1 
  val eof = fn () => Tokens.EOF(!linenum, !colnum)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor A2LexFun(structure Tokens:A2_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (colnum := 1 ; linenum := (!linenum) + 1; lex());
";"      => ( colnum := (!colnum) + 1 ; Tokens.TERM(!linenum,!colnum));
{ws}+    => (colnum := (!colnum) + size yytext ;lex());
"("      => (colnum := (!colnum) + 1 ;Tokens.LPAREN(!linenum,!colnum));
")"      => (colnum := (!colnum) + 1 ;Tokens.RPAREN(!linenum,!colnum));
{alpha}+   => (colnum := (!colnum) + size yytext ;if yytext="IF" then Tokens.IF(!linenum,!colnum) else 
      if yytext="THEN" then Tokens.THEN(!linenum,!colnum) else 
      if yytext="ELSE" then  Tokens.ELSE(!linenum,!colnum) else 
      if yytext="IMPLIES" then Tokens.IMPLIES(!linenum,!colnum) else 
      if yytext="TRUE" then  Tokens.CONST(yytext,!linenum,!colnum) else 
      if yytext="FALSE" then Tokens.CONST(yytext,!linenum,!colnum) else 
      if yytext="AND" then Tokens.AND(!linenum,!colnum) else 
      if yytext="EQUALS" then Tokens.EQUALS(!linenum,!colnum) else 
      if yytext="NOT" then Tokens.NOT(!linenum,!colnum) else 
      if yytext="OR" then Tokens.OR(!linenum,!colnum) else 
      if yytext="XOR" then Tokens.XOR(!linenum,!colnum) else  
      Tokens.ID(yytext,!linenum,!colnum)) ; 



