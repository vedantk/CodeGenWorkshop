CC = clang
CFLAGS = `llvm-config --cppflags`
CXX = clang++
CXXFLAGS = -g -Wall -Wextra -std=c++11 `llvm-config --cppflags`
LDFLAGS = `llvm-config --ldflags --libs all` -lLLVM-3.3

jit: jit.cc parse.tab.o lex.yy.o

lex.yy.o: lex.l
	flex $^
	$(CC) $(CFLAGS) -c lex.yy.c -o $@

parse.tab.o: parse.yy ast.hh
	bison -r all $< -W --defines
	$(CXX) $(CXXFLAGS) -c parse.tab.cc -o $@

clean:
	rm -f jit lex.yy.{c,o} parse.tab.{o,cc,hh} parse.output
