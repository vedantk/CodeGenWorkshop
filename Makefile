all: parse.tab.cc lex.yy.c

parse.tab.cc: parse.yy 
	bison -r all $^ -W --defines

lex.yy.c: lex.l
	flex $^
