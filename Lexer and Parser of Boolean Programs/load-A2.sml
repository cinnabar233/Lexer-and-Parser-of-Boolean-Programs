val args = CommandLine.arguments()
structure A2LrVals = A2LrValsFun(structure Token = LrParser.Token)
structure A2Lex = A2LexFun(structure Tokens = A2LrVals.Tokens);
structure A2Parser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = A2LrVals.ParserData
     	       structure Lex = A2Lex)
fun get_last l = 
    let fun f(x,l) = 
        if l = nil then  x else 
        let val (hd::tl) = l in 
            if Char.isSpace hd then f([],tl) else f(hd::x,tl) 
        end
    in 
        f([],l)

end

fun rev_list l = 
    let fun f(x,l) = 
        if l = nil then x else
            let val (hd::tl) = l in f(hd::x,tl) end
    in f([],l)
end



fun wrong_token s = implode(rev_list(get_last(explode s)))


fun wrong_rule (s) = 
    let val token = wrong_token s 
    in 
        if(token = "ID") then "formula -> ID" else
        if (token = "CONST") then "formula -> CONST" else
        if(token = "IF" orelse token = "THEN" orelse token ="ELSE") then "formula -> IF formula THEN formula ELSE formula" else
        if(token = "AND") then "formula -> formula AND formula" else
        if(token = "NOT") then "formula -> formula NOT formula" else
        if(token = "OR") then "formula -> formula OR formula" else
        if(token = "XOR") then "formula -> formula XOR formula" else
        if(token = "EQUALS") then "formula -> formula EQUALS formula" else
        if(token = "IMPLIES") then "formula -> formula IMPLIES  formula" else
        if(token = "LPAREN" orelse token = "RPAREN") then "formula -> LPAREN formula RPAREN" else
        if(token = "TERM") then "formula -> formula TERM" else
        if (token = "EOF") then "program -> statement" else
        "Unknown Production Rule"
    end


fun invoke lexstream =
    	     	let fun print_error (s,pos1:int,pos2:int) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos1) ^ ":" ^(Int.toString pos2)^":"^ (wrong_rule(s))^ "\n")
		in
		    A2Parser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  A2Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = A2LrVals.Tokens.EOF(0,0)
    	val (result,lexer) = invoke lexer
	val (nextToken, lexer) = A2Parser.Stream.get lexer
    in
        if A2Parser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer


fun create_tokens filename = 
    let 
        val instream = TextIO.openIn filename
        val lexer = SimpLex.makeLexer(fn _ => TextIO.input instream)
        fun create_list(lexer ,l) = 
            let val a = lexer() in if a = SimpLex.UserDeclarations.EOF then l else a :: create_list(lexer,l) end
    in create_list(lexer , nil)
end


fun print_token(SimpLex.UserDeclarations.CONST(y)) = print("CONST "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.AND(y)) = print("AND "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.NOT(y)) = print("NOT "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.OR(y)) = print("OR "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.XOR(y)) = print("XOR "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.IF(y)) = print("IF "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.THEN(y)) = print("THEN "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.ELSE(y)) = print("ELSE "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.ID(y)) = print("ID "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.LPAREN(y)) = print("LPAREN "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.RPAREN(y)) = print("RPAREN "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.TERM(y)) = print("TERM "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.EQUALS(y)) = print("EQUALS "^"\""^y^"\"")
    | print_token(SimpLex.UserDeclarations.EOF) = print("")
    | print_token(SimpLex.UserDeclarations.IMPLIES(y)) = print("IMPLIES "^"\""^y^"\"")



fun print_tokens l = 
        if l = nil then print("")
        else
            let val (hd :: tl) = l 
            in
                (print_token(hd) ; if tl = nil then print("") else print(",") ; print_tokens(tl))
            end;
    
fun print_list l = (print("[") ; print_tokens l ; print("]") );



fun print_lexer_output filename = 
    let val l = create_tokens filename
    in 
        (print("[") ; print_tokens(l) ; print("]\n") )
    end;


fun create_string filename = 
    let 
        val instream = TextIO.inputAll (TextIO.openIn filename) in parseString(instream) end;





fun output (filename) = (print_lexer_output filename; 
    let val s = create_string(filename) in (print("[") ; print(s)  ; print("]\n")) end ) ;  

output(hd(args));
