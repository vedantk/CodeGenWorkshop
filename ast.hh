#pragma once

#include <vector>
#include <stdint.h>
#include <stdlib.h>

#include "llvm/IR/Value.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Instructions.h"

struct Frame
{
    llvm::StringMap<size_t> slots;
    std::vector<llvm::AllocaInst*> bindings;

    Frame(Frame* parent);
    ~Frame();
};

/*
 * The Expr interface helps implement codegen.
 */
struct Expr
{
    virtual ~Expr() {}
    virtual llvm::Value* CodeGen(Frame* frame) = 0;
};

struct Ident : public Expr
{
    llvm::StringRef id;

    Ident(char* _str)
        : id(llvm::StringRef((const char*) _str))
    {}

    virtual llvm::Value* CodeGen(Frame* frame);
};

struct Number : public Expr
{
    int64_t num;

    Number(int64_t n)
        : num(n)
    {}

    virtual llvm::Value* CodeGen(Frame* frame);
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

    virtual llvm::Value* CodeGen(Frame* frame);
};

struct Block : public Expr
{
    std::vector<Expr*> exprs;

    Block() {};

    ~Block()
    {
        for (Expr* e : exprs) {
            delete e;
        }
    }

    virtual llvm::Value* CodeGen(Frame* frame);
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

    virtual llvm::Value* CodeGen(Frame* frame);
};

struct sem_type 
{
    uint32_t func : 1;
    uint32_t arity : 7;
    uint32_t slots : 24;
};

struct VarDef
{
    llvm::StringRef id;
    sem_type type;

    VarDef(char* _name, sem_type _type)
        : id(llvm::StringRef((const char*) _name)), type(_type)
    {}

    ~VarDef()
    {
        free((void*) id.data());
    }
};

struct Parameters
{
    std::vector<VarDef*> params;

    ~Parameters()
    {
        for (VarDef* p : params) {
            delete p;
        }
    }
};

struct FuncDef : public Expr 
{
    Frame* frame;
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

    virtual llvm::Value* CodeGen(Frame* frame);
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

    virtual llvm::Value* CodeGen(Frame* frame);
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

    virtual llvm::Value* CodeGen(Frame* frame);
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

    virtual llvm::Value* CodeGen(Frame* frame);
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

    virtual llvm::Value* CodeGen(Frame* frame);
};

/*
 * The parser generator uses a type union to manage semantic values.
 */
union semval {
    Expr* expr;
    VarDef* vardef;
    Block* exprs;
    Parameters* params;
    sem_type type;
    int podint;
    char* podstr;
};

/*
 * We treat compilation units as functions in order to simplify codegen.
 */
extern FuncDef* Program;
