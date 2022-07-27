gpp_interpreter.out: y.tab.o lex.yy.o 
	gcc -o gpp_interpreter.out y.tab.o lex.yy.o -ll

y.tab.c:gpp_interpreter.y
	yacc -d gpp_interpreter.y

y.tab.o: y.tab.c
	gcc -c y.tab.c 

lex.yy.o: y.tab.h lex.yy.c 
	gcc -c lex.yy.c

lex.yy.c: gpp_lexer.l
	lex gpp_lexer.l

clean:
	rm *.o
