CC = clang
CXX = clang++
CXXFLAGS = -g -Wall -Wextra -std=c++11

jit: jit.cc lex.yy.o parse.tab.o

lex.yy.o: lex.l
	flex $^
	$(CC) -c lex.yy.c -o $@

parse.tab.o: parse.yy lex.yy.o
	bison -r all $< -W --defines
	$(CXX) -c parse.tab.cc -o $@
