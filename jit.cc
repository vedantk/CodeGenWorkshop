#include <string>
#include <stdio.h>

#include "ast.hh"
#include "parse.tab.hh"

#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

using namespace std;
using namespace llvm;

extern "C" FILE* yyin;
extern "C" int yylex(void);

FuncDef* Program;
static Module* TheModule;
static Frame RootClosure;
static ExecutionEngine* Exec;
static IRBuilder<> Builder(getGlobalContext());
static FunctionPassManager* TheFPM;
static Type* Int64Ty = Type::getInt64Ty(getGlobalContext());
static Type* Int64PtrTy = Type::getInt64PtrTy(getGlobalContext());
static Function* MallocF;

/*
 * Cast values into the right format. 
 */
static Value* coerce(Value* V, int level)
{
    switch (level) {
        case 0: return Builder.CreatePtrToInt(V, Int64Ty);
        case 1: 
            if (isa<PointerType>(V->getType())) {
                return Builder.CreatePointerCast(V, Int64PtrTy);
            } else {
                return Builder.CreateIntToPtr(V, Int64PtrTy);
            }
        case 2: 
            if (!isa<PointerType>(V->getType())) {
                V = coerce(V, 1);
            }
            return Builder.CreatePointerCast(V, 
                        PointerType::get(Int64PtrTy, 0));
    }
    printf("coerce: Invalid coercion level\n"); exit(1);
}

static Value* getInt(int64_t n)
{
    return ConstantInt::get(getGlobalContext(), APInt(64, n, true));
}

Value* Ident::CodeGen(Frame* frame)
{
    if (!frame->slots.count(id)) {
        printf("[Ident::CodeGen] Unbound identifier\n"); exit(1);
    }
    Value* V = Builder.CreateLoad(frame->bindings[frame->slots.lookup(id)], id);
    return coerce(V, 1);
}

Value* Number::CodeGen(Frame* frame)
{
    (void) frame;
    return ConstantInt::get(getGlobalContext(), APInt(64, num, true));
}

Value* String::CodeGen(Frame* frame)
{
    (void) frame;
    return ConstantDataArray::getString(getGlobalContext(), 
                                        StringRef((const char*) str));
}

Value* Block::CodeGen(Frame* frame)
{
    for (size_t i=0; i < exprs.size() - 1; ++i) {
        exprs[i]->CodeGen(frame);
    }
    return exprs.back()->CodeGen(frame);
}

Value* FuncCall::CodeGen(Frame* frame)
{
    /* Load the closure. */
    Value* closure = coerce(func->CodeGen(frame), 2);

    /* Load arguments to callee. */
    vector<Value*> ArgsV;
    for (Expr* e : args->exprs) {
        ArgsV.push_back(coerce(e->CodeGen(frame), 1));
    }
    ArgsV.push_back(coerce(closure, 1));

    /* Bitcast the closure to match the call signature. */
    vector<Type*> Pointers(ArgsV.size(), Int64PtrTy);
    FunctionType* FT = FunctionType::get(Int64PtrTy, Pointers, false);
    Value* fptr = Builder.CreateLoad(closure, "loadfptr");
    fptr = Builder.CreateBitCast(fptr, PointerType::get(FT, 0), "fcast");
    return Builder.CreateCall(fptr, ArgsV, "fcall");
}

/*
 * Allocate space for a 64-bit integer in the Function entry block.
 */
static Value* stack_alloc(Frame* frame, Function* F, StringRef id)
{
    if (frame && frame->slots.count(id)) {
        return frame->bindings[frame->slots.lookup(id)];
    }

    IRBuilder<> allocaBuilder(&F->getEntryBlock(), F->getEntryBlock().begin());
    Value* alloca = allocaBuilder.CreateAlloca(Int64Ty, 0, id + "alloca");

    if (frame) {
        frame->slots[id] = frame->bindings.size();
        frame->bindings.push_back(alloca);
    }

    return alloca;
}

/*
 * Make an exact copy of the parent frame.
 */
Frame::Frame(Frame* parent, Value* closure)
{
    closure = coerce(closure, 2);
    for (auto it = parent->slots.begin(); it != parent->slots.end(); ++it) {
        slots[it->getKey()] = it->getValue();
    }
    for (size_t idx=0; idx < parent->bindings.size(); ++idx) {
        /* Bindings are loaded from an i64** into an i64*. */
        bindings.push_back(Builder.CreateConstGEP1_32(coerce(closure, 2), 
                                                      idx+1, "ld"));
    }
}

Value* FuncDef::CodeGen(Frame* parent)
{
    auto ip = Builder.saveIP();

    /* Construct the prototype for the lambda. */
    vector<Type*> Pointers(params->params.size() + 1, Int64PtrTy);
    FunctionType* FT = FunctionType::get(Int64PtrTy, Pointers, false);
    F = Function::Create(FT, Function::ExternalLinkage, "lambda", TheModule);

    /* Assign labels to the function arguments. */
    size_t idx = 0;
    for (auto AI = F->arg_begin(); AI != F->arg_end(); ++AI, ++idx) {
        if (idx < params->params.size()) {
            AI->setName(StringRef(params->params[idx]));
        } else {
            AI->setName(StringRef("__closure__"));
        }
    }

    /* Construct an entry block. */
    BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", F);
    Builder.SetInsertPoint(BB);

    /* Generate a new frame that performs lookups in the enclosing closure. */
    Argument* closure_arg = &F->getArgumentList().back();
    Frame* child = new Frame(parent, closure_arg);

    /* Stack-allocate our parameters and load their Argument values. */
    idx = 0;
    for (auto AI = F->arg_begin(); idx < params->params.size(); ++AI, ++idx) {
        Value* alloc = stack_alloc(child, F, AI->getName());
        Builder.CreateStore(coerce(AI, 0), alloc);
    }

    printf("\nFunction before body generation:\n");
    F->dump();
    printf("\n\n");

    /* Construct the function body. */
    Builder.CreateRet(coerce(block->CodeGen(child), 1));

    printf("Function after body generation:\n");
    F->dump();
    printf("\n\n");

    verifyFunction(*F);
    TheFPM->run(*F);

    Builder.restoreIP(ip);
    return parent->CaptureClosure(F);
}

/*
 * Heap-allocate a closure and copy our frame into it. This becomes the child
 * frame passed into all invocations of a funcdef.
 */
Value* Frame::CaptureClosure(Function* F)
{
    /* Emit a frame directly after a funcdef. */
    int N = bindings.size() + 1;
    Value* falloc = Builder.CreateCall(MallocF, getInt(sizeof(void*) * N), 
                                        "framealloc");

    /* Store cast(F -> i64*) => cast(malloc() -> i64**). */
    Builder.CreateStore(Builder.CreateBitCast(F, Int64PtrTy),
            Builder.CreateBitCast(falloc, PointerType::get(Int64PtrTy, 0)));
    for (int i=1; i < N; ++i) {
        Builder.CreateStore(coerce(Builder.CreateLoad(bindings[i-1], "ld"), 0),
                            Builder.CreateConstGEP1_32(falloc, i, "slot"));
    }
    return falloc;
}

Value* Assignment::CodeGen(Frame* frame)
{
    /* Find (or create!) the reference to our mutable variable. */
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    Value* alloc = stack_alloc(frame, TheFunction, id);

    Value* V = coerce(value->CodeGen(frame), 0);
    Builder.CreateStore(V, alloc);
    return V;
}

Value* UnaryOp::CodeGen(Frame* frame)
{
    Value* V = coerce(arg->CodeGen(frame), 0);
    switch (op) {
        case '!': return Builder.CreateNot(V);
        default:
            printf("[UnaryOp::CodeGen] Unsupported operation\n"); exit(1);
    }
}

Value* BinaryOp::CodeGen(Frame* frame)
{
    Value *L = coerce(lhs->CodeGen(frame), 0), 
          *R = coerce(rhs->CodeGen(frame), 0);
    switch (op) {
        case '+': return Builder.CreateAdd(L, R, "add");
        case '-': return Builder.CreateSub(L, R, "sub");
        case '*': return Builder.CreateMul(L, R, "mul");
        case '/': return Builder.CreateSDiv(L, R, "div");
        case '=': return Builder.CreateICmpEQ(L, R, "eq");
        default: 
            printf("[BinaryOp::CodeGen] Unsupported operation\n"); exit(1);
    }
}

Value* IfElse::CodeGen(Frame* frame)
{
    /* Compare the conditional test expression to 0. */
    Value* cond = Builder.CreateICmpNE(coerce(test->CodeGen(frame), 0), 
                                       getInt(0), "ifcond");

    /* Generate blocks for the consequent, alternate, and phi branches. */
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* ConseqBB = BasicBlock::Create(getGlobalContext(), "conseq",
                                              TheFunction);
    BasicBlock* AlternBB = BasicBlock::Create(getGlobalContext(), "altern");
    BasicBlock* PhiBB = BasicBlock::Create(getGlobalContext(), "ifdone");
    Builder.CreateCondBr(cond, ConseqBB, AlternBB);

    /* Generate the consequent branch and branch to the phi node when done. */
    Builder.SetInsertPoint(ConseqBB);
    Value* conseq = consequent->CodeGen(frame);
    Builder.CreateBr(PhiBB);
    ConseqBB = Builder.GetInsertBlock();

    /* Generate the alternate branch and branch to the phi node when done. */
    TheFunction->getBasicBlockList().push_back(AlternBB);
    Builder.SetInsertPoint(AlternBB);
    Value* altern = alternate->CodeGen(frame);
    Builder.CreateBr(PhiBB);
    AlternBB = Builder.GetInsertBlock();

    /* Wire our parallel branches into the phi node. */
    TheFunction->getBasicBlockList().push_back(PhiBB);
    Builder.SetInsertPoint(PhiBB);
    PHINode* PN = Builder.CreatePHI(Int64Ty, 2, "ifphi");
    PN->addIncoming(conseq, ConseqBB);
    PN->addIncoming(altern, AlternBB);
    return PN;
}

void yyerror(char const* arg)
{
    printf("yyerror: %s\n", arg);
}

void Ident::disp()
{
    printf("(Ident %s)", id.data());
}

void Number::disp()
{
    printf("(Number %ld)", num);
}

void String::disp()
{
    printf("(String %s)", str);
}

void Block::disp()
{
    printf("(Block ");
    for (size_t i=0; i < exprs.size(); ++i) {
        exprs[i]->disp(); 
        if (i < exprs.size() - 1) {
            printf(" ");
        }
    }
    printf(")");
}

void FuncCall::disp()
{
    printf("(FuncCall "); 
    args->disp();
    printf(")");
}

void FuncDef::disp()
{
    printf("(FuncDef (");
    for (char* p : params->params) {
        printf(" %s ", p);
    }
    printf(")\n");
    block->disp();
    printf(")");
}

void Assignment::disp()
{
    printf("(Assignment %s ", id.data()); 
    value->disp(); 
    printf(")");
}

void UnaryOp::disp()
{
    printf("(%c ", op);
    arg->disp();
    printf(")");
}

void BinaryOp::disp()
{
    printf("(%c ", op);
    lhs->disp();
    printf(" ");
    rhs->disp();
    printf(")");
}

void IfElse::disp()
{
    printf("(if ");
    test->disp();
    consequent->disp();
    alternate->disp();
    printf(")");
}

/*
 *      Code has been liberally stolen from the Kaleidoscope tutorial:
 *
 *                      http://llvm.org/docs/tutorial/
 */
int main()
{
    /* Initialize the JIT and set up an optimization pipeline. */
    InitializeNativeTarget();
    LLVMContext &Context = getGlobalContext();

    TheModule = new Module("my cool jit", Context);

    string ErrStr;
    Exec = EngineBuilder(TheModule).setErrorStr(&ErrStr).create();
    if (!Exec) {
        printf("Could not create ExecutionEngine: %s\n", ErrStr.c_str());
        exit(1);
    }

    FunctionPassManager OurFPM(TheModule);
    OurFPM.add(new DataLayout(*Exec->getDataLayout()));
    OurFPM.add(createBasicAliasAnalysisPass());
    OurFPM.add(createPromoteMemoryToRegisterPass());
    OurFPM.add(createInstructionCombiningPass());
    OurFPM.add(createReassociatePass());
    OurFPM.add(createGVNPass());
    OurFPM.add(createCFGSimplificationPass());
    OurFPM.doInitialization();
    TheFPM = &OurFPM;

    /* Link malloc into the module. */
    vector<Type*> MallocArgs(1, Int64Ty);
    FunctionType* MallocFT = FunctionType::get(Int64PtrTy, MallocArgs, false);
    MallocF = Function::Create(MallocFT, Function::ExternalLinkage,
                               "malloc", TheModule);
    MallocF->setCallingConv(CallingConv::C);

    /* Parse, codegen, and execute a program. */
    yyin = stdin;
    if (yyparse()) {
        printf("Parse error.\n"); exit(1);
    }

    if (sizeof(void*) != sizeof(int64_t)) {
        printf("For simplicity, type coercion is not supported "
               "on this architecture.\n"); exit(1);
    }

    printf("Parse:\n");
    Program->disp();
    Program->CodeGen(&RootClosure);
    printf("\n\nLLVM IR:\n");
    TheModule->dump();

    void* entry = Exec->getPointerToFunction(Program->F);
    int64_t (*i64entry)(void*) = (int64_t (*)(void*)) entry;
    int64_t result = i64entry(NULL);
    printf("\n=> %ld\n", result);

    delete Program;
    return 0;
}
