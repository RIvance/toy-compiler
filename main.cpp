#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "ToyVisitor.h"

#include <iostream>
#include <ToyLexer.h>
#include <utility>

#include "common/Stream.hpp"
#include "common/Common.inc"

class CompileError : public std::exception
{
    std::string error;

public:

    explicit CompileError(std::string error) : error(std::move(error)) {}

    [[nodiscard]]
    const char* what() const noexcept override {
        return error.c_str();
    }
};

interface Context
{
    using LLVMLocalVariable = std::tuple<llvm::Type*, llvm::Value*>;
    virtual ~Context() = default;
    virtual def getModule() -> llvm::Module* = 0;
    virtual def getIRBuilder() -> llvm::IRBuilder<>* = 0;
    virtual def registerLocalVariable(const String& ident, llvm::Type* type, llvm::Value* ptr) -> void = 0;
    virtual def removeLocalVariable(const String& ident) -> void = 0;
    [[nodiscard]] virtual def getLocalVariable(const String& ident) const -> LLVMLocalVariable = 0;
    [[nodiscard]] virtual def getFunctionArg(const String& ident) const -> llvm::Value* = 0;
    [[nodiscard]] virtual def getType(const String& ident) const -> llvm::Type* = 0;
    [[nodiscard]] virtual def getCurrentFunction() const -> llvm::Function* = 0;
    virtual def createFunction(const String& ident, llvm::FunctionType* funcType) -> llvm::Function* = 0;
    virtual def exitFunction() -> void = 0;
    virtual def createBasicBlock() -> llvm::BasicBlock* = 0;
    virtual def createBasicBlock(const String& ident) -> llvm::BasicBlock* = 0;
    virtual def createBasicBlock(const String& ident, llvm::Function* fn) -> llvm::BasicBlock* = 0;
};

namespace ast
{
    interface Expr
    {
        virtual ~Expr() = default;
        virtual def emitIR(Context* context) const -> llvm::Value* = 0;
        virtual def getType(Context* context) const -> llvm::Type* = 0;
    };

    struct ExprIntLiteral final : Expr
    {
        Int64 value;
        explicit ExprIntLiteral(Int64 value) : value(value) {}

        def getType(Context* context) const -> llvm::Type* override {
            return context->getIRBuilder()->getInt64Ty();
        }

        def emitIR(Context* context) const -> llvm::ConstantInt* override {
            return context->getIRBuilder()->getInt64(value);
        }
    };

    struct ExprFloatLiteral final : Expr
    {
        Float64 value;
        explicit ExprFloatLiteral(Float64 value) : value(value) {}

        def getType(Context* context) const -> llvm::Type* override {
            return context->getIRBuilder()->getDoubleTy();
        }

        def emitIR(Context* context) const -> llvm::Constant* override {
            return llvm::ConstantFP::get(context->getIRBuilder()->getDoubleTy(), value);
        }
    };

    struct ExprBoolLiteral final : Expr
    {
        Bool value;
        explicit ExprBoolLiteral(Bool value) : value(value) {}

        def getType(Context* context) const -> llvm::Type* override {
            return context->getIRBuilder()->getInt1Ty();
        }

        def emitIR(Context* context) const -> llvm::ConstantInt* override {
            return context->getIRBuilder()->getInt1(value);
        }
    };

    struct ExprCharLiteral final : Expr
    {
        Char value;
        explicit ExprCharLiteral(Char value): value(value) {}

        def getType(Context* context) const -> llvm::Type* override {
            return context->getIRBuilder()->getInt8Ty();
        }

        def emitIR(Context* context) const -> llvm::ConstantInt* override {
            return context->getIRBuilder()->getInt8(value);
        }
    };

    struct ExprStringLiteral final : Expr
    {
        String value;
        explicit ExprStringLiteral(String value): value(std::move(value)) {}

        llvm::Constant* globalStringPtr = nullptr;

        def getType(Context* context) const -> llvm::Type* override {
            return this->getValue(context)->getType();
        }

        def getValue(Context* context) const -> llvm::Constant* {
            if (globalStringPtr != nullptr) {
                return globalStringPtr;
            } else {
                return context->getIRBuilder()->CreateGlobalStringPtr(value);
            }
        }

        def emitIR(Context* context) const -> llvm::Constant* override {
            return getValue(context);
        }
    };

    struct ExprIdentifier final : Expr
    {
        String ident;
        explicit ExprIdentifier(String ident): ident(std::move(ident)) {}

        def getType(Context* context) const -> llvm::Type* override {
            if (let [type, _] = context->getLocalVariable(ident); type != nullptr) {
                return type;
            } else if (let arg = context->getFunctionArg(ident); arg != nullptr) {
                return arg->getType();
            } else {
                throw CompileError("Undefined local variable: " + ident);
            }
        }

        def emitIR(Context* context) const -> llvm::Value* override {
            if (let [type, ptr] = context->getLocalVariable(ident); ptr != nullptr) {
                return context->getIRBuilder()->CreateLoad(type, ptr);
            } else if (let arg = context->getFunctionArg(ident); arg != nullptr) {
                return arg;
            } else {
                throw CompileError("Undefined local variable: " + ident);
            }
        }
    };

    enum class UnaryOp
    {
        Pos, Neg, Not
    };

    struct ExprUnary : Expr
    {
        Box<Expr> expr;
        UnaryOp op;
        ExprUnary(UnaryOp op, Box<Expr>&& expr): expr(std::move(expr)), op(op) {}

        def getType(Context* context) const -> llvm::Type* override {
            return expr->getType(context);
        }

        def emitIR(Context* context) const -> llvm::Value* override {
            let value = expr->emitIR(context);
            switch (op) {
                case UnaryOp::Pos: return value;
                case UnaryOp::Neg: return context->getIRBuilder()->CreateNeg(value);
                case UnaryOp::Not: return context->getIRBuilder()->CreateNot(value);
                default: unreachable();
            }
        }
    };

    enum class BinaryOp
    {
        Add, Sub, Mul, Div, Mod,
        And, Or,
        Eq, Ne, Lt, Gt, Le, Ge
    };

    def binaryOpFromStr(const String& str) -> BinaryOp {
        if (str == "+") {
            return BinaryOp::Add;
        } else if (str == "-") {
            return BinaryOp::Sub;
        } else if (str == "*") {
            return BinaryOp::Mul;
        } else if (str == "/") {
            return BinaryOp::Div;
        } else if (str == "%") {
            return BinaryOp::Mod;
        } else if (str == "&&") {
            return BinaryOp::And;
        } else if (str == "||") {
            return BinaryOp::Or;
        } else if (str == "==") {
            return BinaryOp::Eq;
        } else if (str == "!=") {
            return BinaryOp::Ne;
        } else if (str == "<") {
            return BinaryOp::Lt;
        } else if (str == ">") {
            return BinaryOp::Gt;
        } else if (str == "<=") {
            return BinaryOp::Le;
        } else if (str == ">=") {
            return BinaryOp::Ge;
        } else {
            throw CompileError("Unknown binary operator " + str);
        }
    }

    struct ExprBinary final : Expr
    {
        BinaryOp op;
        Box<Expr> lhs;
        Box<Expr> rhs;
        ExprBinary(
            BinaryOp op, Box<Expr>&& lhs, Box<Expr>&& rhs
        ) : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

        def getType(Context* context) const -> llvm::Type* override {
            let lhsType = lhs->getType(context);
            let rhsType = rhs->getType(context);
            if (lhsType != rhsType) {
                throw CompileError("Binary operation requires operands of the same type");
            }
            return lhsType;
        }

        def emitIR(Context* context) const -> llvm::Value* override {
            let lhsValue = lhs->emitIR(context);
            let rhsValue = rhs->emitIR(context);
            // TODO: type checking
            switch (op) {
                case BinaryOp::Add: return context->getIRBuilder()->CreateAdd(lhsValue, rhsValue);
                case BinaryOp::Sub: return context->getIRBuilder()->CreateSub(lhsValue, rhsValue);
                case BinaryOp::Mul: return context->getIRBuilder()->CreateMul(lhsValue, rhsValue);
                case BinaryOp::Div: return context->getIRBuilder()->CreateSDiv(lhsValue, rhsValue);
                case BinaryOp::Mod: return context->getIRBuilder()->CreateSRem(lhsValue, rhsValue);
                case BinaryOp::And: return context->getIRBuilder()->CreateLogicalAnd(lhsValue, rhsValue);
                case BinaryOp::Or:  return context->getIRBuilder()->CreateLogicalOr(lhsValue, rhsValue);
                case BinaryOp::Eq:  return context->getIRBuilder()->CreateICmpEQ(lhsValue, rhsValue);
                case BinaryOp::Ne:  return context->getIRBuilder()->CreateICmpNE(lhsValue, rhsValue);
                case BinaryOp::Lt:  return context->getIRBuilder()->CreateICmpSLT(lhsValue, rhsValue);
                case BinaryOp::Gt:  return context->getIRBuilder()->CreateICmpSGT(lhsValue, rhsValue);
                case BinaryOp::Le:  return context->getIRBuilder()->CreateICmpSLE(lhsValue, rhsValue);
                case BinaryOp::Ge:  return context->getIRBuilder()->CreateICmpSGE(lhsValue, rhsValue);
                default: unreachable();
            }
        }
    };

    struct ExprCall final : Expr
    {
        String name;
        Vector<Box<Expr>> arguments;

        ExprCall(String name, Vector<Box<Expr>> args) : name(std::move(name)), arguments(std::move(args)) {}

        [[nodiscard]]
        def getCallee(Context* context) const -> llvm::Function* {
            let currentModule = context->getIRBuilder()->GetInsertBlock()->getModule();
            return currentModule->getFunction(name);
        }

        def getType(Context* context) const -> llvm::Type* override {
            if (let callee = getCallee(context); callee != nullptr) {
                return getCallee(context)->getReturnType();
            } else {
                throw CompileError("Undefined function " + name);
            }
        }

        def emitIR(
            Context* context
        ) const -> llvm::CallInst* override {
            let args = Stream(arguments).map<llvm::Value*>([&](const Box<Expr>& arg) -> llvm::Value* {
                return arg->emitIR(context);
            }).collect<Vector<llvm::Value*>>();
            if (let callee = getCallee(context); callee != nullptr) {
                return context->getIRBuilder()->CreateCall(getCallee(context), args);
            } else {
                throw CompileError("Undefined function " + name);
            }
        }
    };

    interface Statement
    {
        virtual ~Statement() = default;
        virtual def emitIR(Context*, Bool isTermination) -> void = 0;
        [[nodiscard]] virtual def isTermination() const -> Bool { return false; }
    };

    struct CodeBlock
    {
        Vector<Box<Statement>> stmts;
        explicit CodeBlock(Vector<Box<Statement>>&& stmts) : stmts(std::move(stmts)) {}

        template <typename T>
        static def fromStatement(T&& statement) -> CodeBlock {
            auto stmts = Vector<Box<Statement>>();
            stmts.push_back(box<T>(std::forward<T>(statement)));
            return CodeBlock(std::move(stmts));
        }

        def emitIR(
            Context* context, llvm::BasicBlock* block = nullptr,
            llvm::BasicBlock* continueBlock = nullptr
        ) const -> llvm::BasicBlock* {
            if (block == nullptr) {
                block = context->createBasicBlock();
            }
            context->getIRBuilder()->SetInsertPoint(block);
            Bool isTerminated = false;
            for (int i = 0; i < stmts.size(); i++) {
                let& stmt = stmts[i];
                stmt->emitIR(context, i == stmts.size() - 1);
                if (stmt->isTermination()) {
                    isTerminated = true;
                    break;
                }
            }
            if (continueBlock != nullptr) {
                if (!isTerminated) context->getIRBuilder()->CreateBr(continueBlock);
                context->getIRBuilder()->SetInsertPoint(continueBlock);
            }
            return block;
        }
    };

    struct StmtExpr final : Statement
    {
        Box<Expr> expr;
        explicit StmtExpr(Box<Expr>&& expr) : expr(std::move(expr)) {}

        def emitIR(Context* context, Bool) -> void override {
            context->getIRBuilder()->Insert(expr->emitIR(context));
        }
    };

    struct StmtReturn final : Statement
    {
        Option<Box<Expr>> expr;
        explicit StmtReturn() : expr(std::nullopt) {}
        explicit StmtReturn(Box<Expr>&& expr) : expr(std::move(expr)) {}

        def emitIR(Context* context, Bool) -> void override {
            if (expr.has_value()) {
                context->getIRBuilder()->CreateRet(expr->get()->emitIR(context));
            } else {
                context->getIRBuilder()->CreateRetVoid();
            }
        }

        [[nodiscard]]
        def isTermination() const -> Bool override {
            return true;
        }
    };

    struct StmtLet final : Statement
    {
        String ident;
        Box<Expr> expr;
        StmtLet(String ident, Box<Expr> expr) : ident(std::move(ident)), expr(std::move(expr)) {}

        def emitIR(Context* context, Bool) -> void override {
            let ptr = context->getIRBuilder()->CreateAlloca(expr->getType(context), nullptr, ident);
            context->getIRBuilder()->CreateStore(expr->emitIR(context), ptr);
            context->registerLocalVariable(ident, expr->getType(context), ptr);
        }
    };

    struct StmtIf final : Statement
    {
        Box<Expr> condition;
        CodeBlock ifBranch;
        Option<CodeBlock> elseBranch;

        StmtIf(
            Box<Expr> condition, CodeBlock ifBranch, Option<CodeBlock> elseBranch
        ): condition(std::move(condition)), ifBranch(std::move(ifBranch)), elseBranch(std::move(elseBranch)) {}

        def emitIR(Context* context, Bool isTermination) -> void override {
            let conditionValue = condition->emitIR(context);
            let ifBranchBlock = context->createBasicBlock();
            let elseBranchBlock = elseBranch.has_value() ? context->createBasicBlock() : nullptr;
            context->getIRBuilder()->CreateCondBr(conditionValue, ifBranchBlock, elseBranchBlock);
            let continueBlock = isTermination ? nullptr : context->createBasicBlock();
            ifBranch.emitIR(context, ifBranchBlock, continueBlock);
            if (elseBranchBlock != nullptr) {
                elseBranch->emitIR(context, elseBranchBlock, continueBlock);
            }
        }
    };

    struct StmtWhile final : Statement
    {
        Box<Expr> condition;
        CodeBlock body;
        StmtWhile(Box<Expr> condition, CodeBlock body): condition(std::move(condition)), body(std::move(body)) {}

        def emitIR(Context* context, Bool isTermination) -> void override {
            TODO();
        }
    };

    struct VariableBinding
    {
        String ident;
        String type;
        VariableBinding(String ident, String type): ident(std::move(ident)), type(std::move(type)) {}
        [[nodiscard]] def isVarArg() const -> Bool { return type == "..."; }
    };

    struct FunctionPrototype
    {
        String ident;
        String returnType;
        Vector<VariableBinding> params;

        FunctionPrototype() = delete;

        explicit FunctionPrototype(
            String ident, String returnType, Vector<VariableBinding> params
        ): ident(std::move(ident)), returnType(std::move(returnType)), params(std::move(params)) {}

        [[nodiscard]]
        def getParamTypes(Context* context) const -> Vector<llvm::Type*> {
            return Stream(params).filter([](const auto& it) {
                return !it.isVarArg();
            }).map<llvm::Type*>([&](const VariableBinding& param) {
                return context->getType(param.type);
            }).collect<Vector<llvm::Type*>>();
        }

        [[nodiscard]]
        def isVarArg() const -> Bool {
            return !params.empty() && params.back().isVarArg();
        }
    };

    struct Function : FunctionPrototype
    {
        CodeBlock body;

        Function(
            const String& ident, const String& returnType, Vector<VariableBinding> params, CodeBlock body
        ): FunctionPrototype(ident, returnType, std::move(params)), body(std::move(body)) {}

        def emitIR(Context* context) const {
            let paramTypes = this->getParamTypes(context);
            let fnType = llvm::FunctionType::get(context->getType(returnType), paramTypes, this->isVarArg());
            let fn = context->createFunction(ident, fnType);
            for (int i = 0; i < fn->arg_size(); i++) {
                let arg = fn->getArg(i);
                arg->setName(params[i].ident);
            }
            body.emitIR(context);
        }
    };

    struct ExternFunction : FunctionPrototype
    {
        def emitIR(Context* context) const {
            let paramTypes = this->getParamTypes(context);
            let fnType = llvm::FunctionType::get(context->getType(returnType), paramTypes, this->isVarArg());
            context->createFunction(ident, fnType);
        }

        ExternFunction(
            const String& ident, const String& returnType, Vector<VariableBinding> params
        ): FunctionPrototype(ident, returnType, std::move(params)) {}
    };

    struct Module
    {
        Vector<Function> functions;
        Vector<ExternFunction> externFunctions;

        Module(Vector<Function>&& functions, Vector<ExternFunction>&& externFunctions)
            : functions(std::move(functions)),
            externFunctions(std::move(externFunctions)) {
        }

        void emitIR(Context* context) {
            for (auto& function : externFunctions) {
                function.emitIR(context);
            }
            for (auto& function : functions) {
                function.emitIR(context);
            }
        }
    };
}

class ToyVisitor
{
public:

    def visitFile(toylang::ToyParser::FileContext* context) -> ast::Module {
        using toylang::ToyParser;
        return {
            Stream(context->funcDefinitions).map<ast::Function>([this](const ToyParser::FuncDefContext* func) {
                return visitFuncDef(const_cast<ToyParser::FuncDefContext*>(func));
            }).collect<Vector<ast::Function>>(),
            Stream(context->funcDeclarations).map<ast::ExternFunction>([this](const ToyParser::FuncDeclContext* func) {
                return visitFuncDecl(const_cast<ToyParser::FuncDeclContext*>(func));
            }).collect<Vector<ast::ExternFunction>>()
        };
    }

    def visitFuncDecl(toylang::ToyParser::FuncDeclContext* context) -> ast::ExternFunction {
        return {
            context->ident->getText(),
            context->returnType->getText(),
            visitParamList(context->paramList())
        };
    }

    def visitFuncDef(toylang::ToyParser::FuncDefContext* context) -> ast::Function {
        return {
            context->ident->getText(),
            context->returnType ? context->returnType->getText() : "Unit",
            visitParamList(context->paramList()),
            visitBlock(context->block())
        };
    }

    def visitParamList(toylang::ToyParser::ParamListContext* context) -> Vector<ast::VariableBinding> {
        using toylang::ToyParser;
        return Stream(context->params).map<ast::VariableBinding>([](auto param) {
            if (let paramDecl = dynamic_cast<ToyParser::ParamDeclContext*>(param)) {
                return ast::VariableBinding(paramDecl->ident->getText(), paramDecl->type->getText());
            } else {
                return ast::VariableBinding("", "...");
            }
        }).collect<Vector<ast::VariableBinding>>();
    }

    def visitBlock(toylang::ToyParser::BlockContext* context) -> ast::CodeBlock {
        return ast::CodeBlock(
            Stream(context->stmts).map<Box<ast::Statement>>([this](const toylang::ToyParser::StatementContext* stmt) {
                return visitStmt(const_cast<toylang::ToyParser::StatementContext*>(stmt));
            }).collect<Vector<Box<ast::Statement>>>()
        );
    }

    def visitStmt(toylang::ToyParser::StatementContext* context) -> Box<ast::Statement> {
        using toylang::ToyParser;
        if (auto* stmt = dynamic_cast<ToyParser::StmtExprContext*>(context)) {
            return box<ast::StmtExpr>(visitStmtExpr(stmt));
        } else if (auto* stmt = dynamic_cast<ToyParser::StmtReturnContext*>(context)) {
            return box<ast::StmtReturn>(visitStmtReturn(stmt));
        } else if (auto* stmt = dynamic_cast<ToyParser::StmtLetContext*>(context)) {
            return box<ast::StmtLet>(visitStmtLet(stmt));
        } else if (auto* stmt = dynamic_cast<ToyParser::StmtIfContext*>(context)) {
            return box<ast::StmtIf>(visitStmtIf(stmt));
        } else if (auto* stmt = dynamic_cast<ToyParser::StmtWhileContext*>(context)) {
            return box<ast::StmtWhile>(visitStmtWhile(stmt));
        }
        unreachable();
    }

    def visitStmtExpr(toylang::ToyParser::StmtExprContext* context) -> ast::StmtExpr {
        return ast::StmtExpr(visitExpr(context->expr));
    }

    def visitStmtReturn(toylang::ToyParser::StmtReturnContext* context) -> ast::StmtReturn {
        if (context->expr == nullptr) {
            return ast::StmtReturn();
        } else {
            return ast::StmtReturn(visitExpr(context->expr));
        }
    }

    def visitStmtLet(toylang::ToyParser::StmtLetContext* context) -> ast::StmtLet {
        return ast::StmtLet(context->ident->getText(), visitExpr(context->expr));
    }

    def visitIfStatement(toylang::ToyParser::IfStatementContext* context) -> ast::StmtIf {
        return ast::StmtIf(
            visitExpr(context->condition),
            visitBlock(context->ifBranch),
            context->elseBranch != nullptr ? (
                std::make_optional(visitBlock(context->elseBranch))
            ) : context->elseIf != nullptr ? (
                std::make_optional(ast::CodeBlock::fromStatement(visitIfStatement(context->elseIf)))
            ) : std::nullopt
        );
    }

    def visitStmtIf(toylang::ToyParser::StmtIfContext* context) -> ast::StmtIf {
        return visitIfStatement(context->stmt);
    }

    def visitStmtWhile(toylang::ToyParser::StmtWhileContext* context) -> ast::StmtWhile {
        TODO();
    }

    def visitExpr(toylang::ToyParser::ExpressionContext* context) -> Box<ast::Expr> {
        using toylang::ToyParser;
        if (auto* expr = dynamic_cast<ToyParser::ExprIntLiteralContext*>(context)) {
            return box<ast::ExprIntLiteral>(visitExprIntLiteral(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprFloatLiteralContext*>(context)) {
            return box<ast::ExprFloatLiteral>(visitExprFloatLiteral(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprBoolLiteralContext*>(context)) {
            return box<ast::ExprBoolLiteral>(visitExprBoolLiteral(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprCharLiteralContext*>(context)) {
            return box<ast::ExprCharLiteral>(visitExprCharLiteral(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprStringLiteralContext*>(context)) {
            return box<ast::ExprStringLiteral>(visitExprStringLiteral(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprIdentifierContext*>(context)) {
            return box<ast::ExprIdentifier>(visitExprIdentifier(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprParenContext*>(context)) {
            return visitExpr(expr->expr);
        } else if (auto* expr = dynamic_cast<ToyParser::ExprCallContext*>(context)) {
            return box<ast::ExprCall>(visitExprCall(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprMulDivModContext*>(context)) {
            return box<ast::ExprBinary>(visitExprMulDivMod(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprAddSubContext*>(context)) {
            return box<ast::ExprBinary>(visitExprAddSub(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprComparisonContext*>(context)) {
            return box<ast::ExprBinary>(visitExprComparison(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprLogicalContext*>(context)) {
            return box<ast::ExprBinary>(visitExprLogical(expr));
        } else if (auto* expr = dynamic_cast<ToyParser::ExprPosNegContext*>(context)) {
            return box<ast::ExprUnary>(visitExprPosNeg(expr));
        }
        unreachable();
    }

    def visitExprIntLiteral(toylang::ToyParser::ExprIntLiteralContext* context) -> ast::ExprIntLiteral {
        return ast::ExprIntLiteral(std::stoll(context->getText()));
    }

    def visitExprFloatLiteral(toylang::ToyParser::ExprFloatLiteralContext* context) -> ast::ExprFloatLiteral {
        return ast::ExprFloatLiteral(std::stod(context->getText()));
    }

    def visitExprBoolLiteral(toylang::ToyParser::ExprBoolLiteralContext* context) -> ast::ExprBoolLiteral {
        if (context->getText() == "true") {
            return ast::ExprBoolLiteral(true);
        } else if (context->getText() == "false") {
            return ast::ExprBoolLiteral(false);
        } else {
            throw std::exception();
        }
    }

    def visitExprCharLiteral(toylang::ToyParser::ExprCharLiteralContext* context) -> ast::ExprCharLiteral {
        return ast::ExprCharLiteral(context->getText()[0]);
    }

    def visitExprStringLiteral(toylang::ToyParser::ExprStringLiteralContext* context) -> ast::ExprStringLiteral {
        return ast::ExprStringLiteral(context->getText().substr(1, context->getText().length() - 2));
    }

    def visitExprIdentifier(toylang::ToyParser::ExprIdentifierContext* context) -> ast::ExprIdentifier {
        return ast::ExprIdentifier(context->getText());
    }

    def visitExprParen(toylang::ToyParser::ExprParenContext* context) -> Box<ast::Expr> {
        return visitExpr(context->expr);
    }

    def visitExprCall(toylang::ToyParser::ExprCallContext* context) -> ast::ExprCall {
        using toylang::ToyParser;
        return {
            context->ident->getText(),
            Stream(context->args).map<Box<ast::Expr>>([this](const ToyParser::ExpressionContext* expr) {
                return visitExpr(const_cast<ToyParser::ExpressionContext*>(expr));
            }).collect<Vector<Box<ast::Expr>>>()
        };
    }

    def visitExprMulDivMod(toylang::ToyParser::ExprMulDivModContext* context) -> ast::ExprBinary {
        return ast::ExprBinary(
            ast::binaryOpFromStr(context->op->getText()), visitExpr(context->lhs), visitExpr(context->rhs)
        );
    }

    def visitExprAddSub(toylang::ToyParser::ExprAddSubContext* context) -> ast::ExprBinary {
        return ast::ExprBinary(
            ast::binaryOpFromStr(context->op->getText()), visitExpr(context->lhs), visitExpr(context->rhs)
        );
    }

    def visitExprComparison(toylang::ToyParser::ExprComparisonContext* context) -> ast::ExprBinary {
        return ast::ExprBinary(
            ast::binaryOpFromStr(context->op->getText()), visitExpr(context->lhs), visitExpr(context->rhs)
        );
    }

    def visitExprLogical(toylang::ToyParser::ExprLogicalContext* context) -> ast::ExprBinary {
        return ast::ExprBinary(
            ast::binaryOpFromStr(context->op->getText()), visitExpr(context->lhs), visitExpr(context->rhs)
        );
    }

    def visitExprPosNeg(toylang::ToyParser::ExprPosNegContext* context) -> ast::ExprUnary {
        return ast::ExprUnary(
            context->op->getText() == "+" ? ast::UnaryOp::Pos : ast::UnaryOp::Neg,
            visitExpr(context->expr)
        );
    }

    def visitExprNot(toylang::ToyParser::ExprNotContext* context) -> ast::ExprUnary {
        return ast::ExprUnary(ast::UnaryOp::Not, visitExpr(context->expr));
    }
};

class CodeGenContext final : public Context
{
    llvm::LLVMContext context = llvm::LLVMContext();
    mutable llvm::IRBuilder<> builder = llvm::IRBuilder(context);
    llvm::Module module = llvm::Module("MainModule", context);

    HashMap<String, LLVMLocalVariable> localVariables;
    llvm::Function* currentFunction = nullptr;

public:

    [[nodiscard]]
    def getIRBuilder() -> llvm::IRBuilder<>* override {
        return &builder;
    }

    [[nodiscard]]
    def getModule() -> llvm::Module* override {
        return &module;
    }

    [[nodiscard]]
    def getLocalVariable(const String& name) const -> LLVMLocalVariable override {
        if (let iter = localVariables.find(name); iter != localVariables.end()) {
            return iter->second;
        } else {
            return { nullptr, nullptr };
        }
    }

    def registerLocalVariable(const String& ident, llvm::Type* type, llvm::Value* ptr) -> void override {
        localVariables[ident] = { type, ptr };
    }

    def removeLocalVariable(const String& ident) -> void override {
        localVariables.erase(ident);
    }

    [[nodiscard]]
    def getType(const String& ident) const -> llvm::Type* override {
        if (ident == "Bool") {
            return builder.getInt1Ty();
        } else if (ident == "Int") {
            return builder.getInt64Ty();
        } else if (ident == "Float") {
            return builder.getDoubleTy();
        } else if (ident == "Char") {
            return builder.getInt8Ty();
        } else if (ident == "String") {
            return builder.getInt8PtrTy();
        } else if (ident == "Unit") {
            return builder.getVoidTy();
        } else if (ident == "...") {
            return builder.getPtrTy();
        } else {
            throw CompileError("Unknown type " + ident);
        }
    }

    def getCurrentFunction() const -> llvm::Function* override {
        return this->currentFunction;
    }

    def getFunctionArg(const String& ident) const -> llvm::Value* override {
        for (auto& arg : currentFunction->args()) {
            if (arg.getName() == ident) { return &arg; }
        }
        return nullptr;
    }

    def createFunction(const String& ident, llvm::FunctionType* funcType) -> llvm::Function* override {
        if (module.getFunction(ident) != nullptr) {
            throw CompileError("Function " + ident + " already exists");
        }
        auto fn = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, ident, &module);
        this->currentFunction = fn;
        llvm::verifyFunction(*fn);
        return fn;
    }

    def exitFunction() -> void override {
        localVariables.clear();
        this->currentFunction = nullptr;
    }

    def createBasicBlock() -> llvm::BasicBlock* override {
        return llvm::BasicBlock::Create(context, "", currentFunction);
    }

    def createBasicBlock(const String& ident, llvm::Function* fn) -> llvm::BasicBlock* override {
        return llvm::BasicBlock::Create(context, ident, fn);
    }

    def createBasicBlock(const String& ident) -> llvm::BasicBlock* override {
        return llvm::BasicBlock::Create(context, ident, currentFunction);
    }
};

def emitModuleIR(const llvm::Module& module, llvm::AssemblyAnnotationWriter* writer = nullptr) -> String {
    String irString;
    llvm::raw_string_ostream ostream(irString);
    module.print(ostream, writer);
    return irString;
}

def saveToFile(const String& path, const String& content) {
    std::ofstream file(path);
    file << content;
    file.close();
}

def main(int argc, const char* argv[]) -> Int32 {

    if (argc < 2) {
        std::cout << "Usage: " << argv[0] << " <file>" << std::endl;
    }

    let file = String(argv[1]);
    antlr4::ANTLRFileStream inputStream;
    inputStream.loadFromFile(file);

    auto context = CodeGenContext();
    auto visitor = ToyVisitor();

    auto lexer = toylang::ToyLexer(&inputStream);
    auto tokens = antlr4::CommonTokenStream(&lexer);
    auto parser = toylang::ToyParser(&tokens);
    auto fileAst = parser.file();

    visitor.visitFile(fileAst).emitIR(&context);
    let ir = emitModuleIR(*context.getModule());

    std::cout << ir << std::endl;
    saveToFile("main.ll", ir);

    system("llc main.ll --relocation-model=pic -filetype=obj -o main.o");
    system("clang main.o -o a.out");

    return 0;
}
