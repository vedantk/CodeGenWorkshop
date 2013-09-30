#include <stdio.h>
#include <stdlib.h>
#include <string>

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
 * Arithmetic and comparisons require matching types. We can lower
 * some of our expressions into qwords for simplicity.
 */
static Value* lower(Expr* e, Frame* frame)
{
    return Builder.CreateIntCast(e->CodeGen(frame), Int64Ty, true, "lower");
}

/*
 * Function arguments and closures need to behave predictably.
 */
static Value* raise(Expr* e, Frame* frame)
{
    return Builder.CreatePointerCast(e->CodeGen(frame), Int64PtrTy, "raise");
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
    return Builder.CreateLoad(frame->bindings[frame->slots.lookup(id)], id);
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
    Value* closure = raise(func, frame);

    /* Load arguments to callee. */
    vector<Value*> ArgsV;
    for (Expr* e : args->exprs) {
        ArgsV.push_back(raise(e, frame));
    }
    ArgsV.push_back(closure);

    /* Bitcast the closure to match the call signature. */
    vector<Type*> Pointers(ArgsV.size(), Int64PtrTy);
    FunctionType* FT = FunctionType::get(Int64PtrTy, Pointers, false);
    Value* fptr = Builder.CreateBitCast(closure, FT, "fcast");

    return Builder.CreateCall(fptr, ArgsV, "fcall");
}

void Frame::InjectBinding(llvm::Value* AI, StringRef id)
{
    slots[id] = bindings.size();
    bindings.push_back(AI);
}

/*
 * Allocate space for a 64-bit integer in the Function entry block.
 */
static Value* stack_alloc(Frame* frame, Function* F, StringRef id)
{
    if (frame->slots.count(id)) {
        return frame->bindings[frame->slots.lookup(id)];
    }

    IRBuilder<> allocaBuilder(&F->getEntryBlock(), F->getEntryBlock().begin());
    Value* alloca = allocaBuilder.CreateAlloca(Int64Ty, 0, id);
    frame->InjectBinding(alloca, id);
    return alloca;
}

Frame::Frame(Frame* parent, Value* closure)
{
    for (auto it = parent->slots.begin(); it != parent->slots.end(); ++it) {
        slots[it->getKey()] = it->getValue();
    }
    for (size_t idx=0; idx < parent->bindings.size(); ++idx) {
        bindings.push_back(Builder.CreateConstGEP1_32(closure, idx+1, "ld"));
    }
}

/*
 * Heap-allocate a closure and copy our frame into it.
 */
Value* Frame::CaptureClosure(Function* F)
{
    int N = bindings.size() + 1;
    Value* closure = Builder.CreateCall(MallocF, getInt(N), "framealloc");
    Builder.CreateStore(Builder.CreateBitCast(F, Int64PtrTy), closure);
    for (int i=1; i < N; ++i) {
        Builder.CreateStore(Builder.CreateLoad(bindings[i-1], "ld"),
                            Builder.CreateConstGEP1_32(closure, i, "binding"));
    }
    return closure;
}

Value* FuncDef::CodeGen(Frame* parent)
{
    /* Construct the prototype for the lambda. */
    vector<Type*> Pointers(params->params.size() + 1, Int64PtrTy);
    FunctionType* FT = FunctionType::get(Int64PtrTy, Pointers, false);
    F = Function::Create(FT, Function::ExternalLinkage, "lambda", TheModule);

    /* Construct an entry block. */
    BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", F);
    Builder.SetInsertPoint(BB);

    /* Generate a new frame that performs lookups in the enclosing closure. */
    Value* env = &F->getArgumentList().back();
    Frame* child = new Frame(parent, env);

    /* Stack-allocate our parameters and load their Argument values. */
    size_t idx = 0;
    for (auto AI = F->arg_begin(); AI != F->arg_end(); ++AI, ++idx) {
        StringRef name = idx < params->params.size() ?
            StringRef(params->params[idx]) : StringRef("$closure");
        AI->setName(name);
        Value* alloc = stack_alloc(child, F, name);
        Builder.CreateStore(AI, alloc);
    }

    /* Construct the function body. */
    Builder.CreateRet(raise(block, child));
    verifyFunction(*F);
    TheFPM->run(*F);

    return parent->CaptureClosure(F);
}

Value* Assignment::CodeGen(Frame* frame)
{
    /* Find (or create!) the reference to our mutable variable. */
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    Value* alloc = stack_alloc(frame, TheFunction, id);

    /* Store into the alloca. */
    Value* V = lower(value, frame);
    Builder.CreateStore(V, alloc);
    return V;
}

Value* UnaryOp::CodeGen(Frame* frame)
{
    Value* V = lower(arg, frame);
    switch (op) {
        case '!': return Builder.CreateNot(V);
        default:
            printf("[UnaryOp::CodeGen] Unsupported operation\n"); exit(1);
    }
}

Value* BinaryOp::CodeGen(Frame* frame)
{
    Value *L = lower(lhs, frame), 
          *R = lower(rhs, frame);
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
    Value* cond = Builder.CreateICmpNE(lower(test, frame), getInt(0), "ifcond");

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

/*
 * Code has been liberally stolen from the Kaleidoscope tutorial:
 *
 *                  http://llvm.org/docs/tutorial/
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
    yyparse();
    Program->CodeGen(&RootClosure);
    void* entry = Exec->getPointerToFunction(Program->F);
    int64_t (*i64entry)(void*) = (int64_t (*)(void*)) entry;
    int64_t result = i64entry(NULL);
    printf(":: %ld\n", result);

    TheModule->dump();

    delete Program;
    return 0;
}
