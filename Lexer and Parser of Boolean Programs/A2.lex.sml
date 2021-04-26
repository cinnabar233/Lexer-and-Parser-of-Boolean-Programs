(*#line 17.10 "A2.lex"*)functor A2LexFun(structure Tokens:A2_TOKENS)(*#line 1.1 "A2.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "A2.lex"*)structure Tokens= Tokens
  
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
  
(*#line 21.1 "A2.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\007\008\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\007\000\000\000\000\000\000\000\006\005\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000"
),
 (3, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 13)], trans = 3},
{fin = [(N 3)], trans = 0},
{fin = [(N 10)], trans = 0},
{fin = [(N 8)], trans = 0},
{fin = [(N 6)], trans = 7},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((*#line 23.14 "A2.lex"*)colnum := 1 ; linenum := (!linenum) + 1; lex()(*#line 135.1 "A2.lex.sml"*)
)
| 10 => ((*#line 27.14 "A2.lex"*)colnum := (!colnum) + 1 ;Tokens.RPAREN(!linenum,!colnum)(*#line 137.1 "A2.lex.sml"*)
)
| 13 => let val yytext=yymktext() in (*#line 28.16 "A2.lex"*)colnum := (!colnum) + size yytext ;if yytext="IF" then Tokens.IF(!linenum,!colnum) else 
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
      Tokens.ID(yytext,!linenum,!colnum)(*#line 150.1 "A2.lex.sml"*)
 end
| 3 => ((*#line 24.14 "A2.lex"*) colnum := (!colnum) + 1 ; Tokens.TERM(!linenum,!colnum)(*#line 152.1 "A2.lex.sml"*)
)
| 6 => let val yytext=yymktext() in (*#line 25.14 "A2.lex"*)colnum := (!colnum) + size yytext ;lex()(*#line 154.1 "A2.lex.sml"*)
 end
| 8 => ((*#line 26.14 "A2.lex"*)colnum := (!colnum) + 1 ;Tokens.LPAREN(!linenum,!colnum)(*#line 156.1 "A2.lex.sml"*)
)
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
