structure SimpLex=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "A2_1.lex"*)datatype lexresult= CONST of string |NOT of string | AND of string| OR of string | XOR of string | EQUALS  of string | IMPLIES of string |IF of string |THEN of string |ELSE of string| ID of string| RPAREN of string | LPAREN of string | EOF  | TERM of string 
val linenum = ref 1
val colnum = ref 0 
val flag = ref false
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => if !flag then OS.Process.exit(OS.Process.success) else EOF
fun refinc x  =  (x := !x + 1; !x)
exception UnknownToken of int*int*string ;
(*#line 13.1 "A2_1.lex.sml"*)
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
"\003\003\003\003\003\003\003\003\003\009\011\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\009\003\003\003\003\003\003\003\008\007\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\006\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
{fin = [(N 15)], trans = 0},
{fin = [(N 13),(N 15)], trans = 4},
{fin = [(N 13)], trans = 4},
{fin = [(N 6),(N 15)], trans = 0},
{fin = [(N 10),(N 15)], trans = 0},
{fin = [(N 8),(N 15)], trans = 0},
{fin = [(N 4),(N 15)], trans = 9},
{fin = [(N 4)], trans = 9},
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

  1 => ((*#line 16.8 "A2_1.lex"*)colnum := 0 ; refinc linenum ; lex() (*#line 130.1 "A2_1.lex.sml"*)
)
| 10 => ((*#line 20.9 "A2_1.lex"*)refinc colnum ; RPAREN ")"(*#line 132.1 "A2_1.lex.sml"*)
)
| 13 => let val yytext=yymktext() in (*#line 21.14 "A2_1.lex"*)colnum := !colnum + size(yytext) ; if yytext="IF" then IF "IF" else 
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
			ID yytext(*#line 145.1 "A2_1.lex.sml"*)
 end
| 15 => let val yytext=yymktext() in (*#line 33.7 "A2_1.lex"*)refinc colnum ; (flag := true ) ; error("Unknown Token:"^Int.toString(!linenum)^":"^Int.toString(!colnum)^":"^yytext) ; lex()(*#line 147.1 "A2_1.lex.sml"*)
 end
| 4 => let val yytext=yymktext() in (*#line 17.11 "A2_1.lex"*)colnum := (!colnum)+size yytext; lex()(*#line 149.1 "A2_1.lex.sml"*)
 end
| 6 => ((*#line 18.9 "A2_1.lex"*)refinc colnum ; TERM ";"(*#line 151.1 "A2_1.lex.sml"*)
)
| 8 => ((*#line 19.9 "A2_1.lex"*)refinc colnum ; LPAREN "("(*#line 153.1 "A2_1.lex.sml"*)
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
