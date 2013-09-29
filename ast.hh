#pragma once

#include <stdint.h>
#include <stdlib.h>

#include <list>
#include <llvm/IR/Value.h>

using namespace std;

struct Expr
{
    virtual ~Expr() {}
    virtual llvm::Value* CodeGen() { return NULL; }
};

struct Block : public Expr
{
    list<Expr*> exprs;

    Block() {};

    ~Block()
    {
        for (Expr* e : exprs) {
            delete e;
        }
    }

    llvm::Value* CodeGen();
};

struct FuncCall : public Expr 
{
    Expr* func;
    Block* args;

    FuncCall(Expr* _func, Block* _args)
        : func(_func), args(_args)
    {}

    ~FuncCall()
    {
        delete func;
        delete args;
    }

    llvm::Value* CodeGen();
};

struct sem_type 
{
    uint32_t func : 1;
    uint32_t arity : 7;
    uint32_t slots : 24;
};

struct VarDef
{
    char* name;
    sem_type type;

    VarDef(char* _name, sem_type _type)
        : name(_name), type(_type)
    {}

    ~VarDef()
    {
        free(name);
    }
};

struct Parameters
{
    list<VarDef*> params;

    ~Parameters()
    {
        for (VarDef* p : params) {
            delete p;
        }
    }
};

struct FuncDef : public Expr 
{
    Parameters* params;
    Block* block;

    FuncDef(Parameters* _params, Block* _block)
        : params(_params), block(_block)
    {}

    ~FuncDef()
    {
        delete params;
        delete block;
    }

    llvm::Value* CodeGen();
};

struct Assignment : public Expr
{
    VarDef* decl;
    Expr* value;

    Assignment(VarDef* _decl, Expr* _value)
        : decl(_decl), value(_value)
    {}

    ~Assignment()
    {
        delete decl;
        delete value;
    }

    llvm::Value* CodeGen();
};

struct UnaryOp : public Expr 
{
    char op;
    Expr* arg;

    UnaryOp(char _op, Expr* _arg)
        : op(_op), arg(_arg)
    {}

    ~UnaryOp()
    {
        delete arg;
    }

    llvm::Value* CodeGen();
};

struct BinaryOp : public Expr
{
    char op;
    Expr *lhs, *rhs;

    BinaryOp(char _op, Expr* _lhs, Expr* _rhs)
        : op(_op), lhs(_lhs), rhs(_rhs)
    {}

    ~BinaryOp()
    {
        delete lhs;
        delete rhs;
    }

    llvm::Value* CodeGen();
};

struct IfElse : public Expr
{
    Expr *test; 
    Block *consequent, *alternate;

    IfElse(Expr* _test, Block* _conseq, Block* _altern)
        : test(_test), consequent(_conseq), alternate(_altern)
    {}

    ~IfElse()
    {
        delete test;
        delete consequent;
        delete alternate;
    }

    llvm::Value* CodeGen();
};

struct Number : public Expr
{
    int64_t num;

    Number(int64_t n)
        : num(n)
    {}

    llvm::Value* CodeGen();
};

struct String : public Expr
{
    char* str;

    String(char* _str)
        : str(_str)
    {}

    ~String() 
    {
        free(str);
    }

    llvm::Value* CodeGen();
};

union semval {
	Expr* expr;
	Number* num;
	String* str;
	IfElse* ifelse;
	FuncCall* fcall;
	BinaryOp* binop;
	UnaryOp* unop;
	FuncDef* funcdef;
	Assignment* asgn;
	VarDef* vardef;
	Block* exprs;
    Parameters* params;
    sem_type type;
    int podint;
    char* podstr;
};

extern Block* program;
