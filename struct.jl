isName(arg) = arg isa Symbol
isIncomplete(exp) = exp isa Expr && exp.head == Symbol("incomplete")

mutable struct Variable
    name
    value
end

mutable struct Frame
    bindings::Vector{Variable}
end

mutable struct Environment
    frames::Vector{Frame}
end

isGlobal(arg) = arg isa Expr && arg.head == :global

struct Block
    els::Vector
end
isBlock(arg) = arg isa Expr && arg.head == :block
toBlock(node) = Block(node.args)

struct Primitive
    function_name::Symbol
end


abstract type Op end

struct ArithmeticOp <: Op
    operation::String
end

struct LogicalOp <: Op
    operation::String
end

struct Let
    names
    inits
    body
end
isLet(arg) = arg isa Expr && arg.head == :let
toLet(node) = Let(letNames(node), letInits(node), node.args[2].args)
function letInits(exp::Expr)
    if (exp.args[1].head == :block)
        map(x -> x.args[2], exp.args[1].args)
    else
        [exp.args[1].args[2]]
    end
end
function letNames(exp::Expr)
    if(exp.args[1].head == :block)  
        map(x -> x.args[1], exp.args[1].args)
    else
        [exp.args[1].args[1]]
    end
end

struct Call
    fn
    args::Vector
end
isCall(arg) = arg isa Expr && arg.head == :call
toCall(node) = Call(node.args[1], node.args[2:end])
isAnon(node) = node isa Expr && node.head == Symbol("->")

struct If
    test::Expr
    thenBlock::Any
    elseBlock::Any
end
isIf(arg) = arg isa Expr && (arg.head == :if || arg.head == :elseif)
function toIf(node::Expr)
    test = node.args[1]
    thenBlock = node.args[2]
    elseBlock = length(node.args) > 2 ? node.args[3] : nothing

    if isIf(elseBlock)
        elseBlock = toIf(elseBlock)
    end
    return If(test, thenBlock, elseBlock)
end

struct Equal
    left
    init
end
isEqual(arg) = arg.head == Symbol("=") || arg.head == Symbol(":=")
toEqual(node) = Equal(node.args[1], node.args[2])

struct FuncDef
    name
    params::Vector
    body
    env::Environment
end
Base.show(io::IO, f::FuncDef) = print(io, "<function>")
function toAnon(node, env)
    if (node.args[1] isa Symbol)
        # capt the env at closure def
        fnDef = FuncDef(Symbol("->"), [node.args[1]], node.args[2].args, Environment(copy(env.frames)))
    else
        fnDef = FuncDef(Symbol("->"), node.args[1].args, node.args[2].args, Environment(copy(env.frames)))
    end
    fnDef
end

struct Fexpr
    name
    params::Vector
    body
    env::Environment
end
Base.show(io::IO, fnDef::Fexpr) = print(io, "<fexpr>")
struct FuncCall
    params::Vector
    call
end

isOr(exp::Expr) = (exp isa Expr && exp.head == :||)
isAnd(exp::Expr) = (exp isa Expr && exp.head == :&&)
isQuoteNode(exp) = exp isa QuoteNode || (exp isa Expr && exp.head == :quote)
Base.show(io::IO, s::Symbol) = print(io, s) # :foo -> foo
Base.show(io::IO, e::Expr) = print(io, e) # :(foo + bar) -> foo + bar

struct Macro
    name
    params::Vector
    call
    env::Environment  
end
Base.show(io::IO, macroo::Macro) = print(io, "<macro>")
isMacro(exp::Expr) = exp.head == :$=

function toMacro(node, env)
    if length(node.args) != 2 || !(node.args[1].head == :call)
        error("Invalid macro expression")
    end
    name = node.args[1].args[1]
    params = [node.args[1].args[2], node.args[1].args[3]]
    call = node.args[2]
    macro_env = Environment([Frame(copy(env.frames[1].bindings))]) 
    return Macro(name, params, call, macro_env)
end
isIncomplete(exp::Expr) = exp.head == Symbol("incomplete")

function isEvalCall(node)
    if (node isa Expr) #array of Expr 
        if (node.args[1] == Symbol("eval")) #the first element is 'eval(expr)'
            return true
        end
    end
    return false
end

function isPrintln(node)
    if (node isa Expr)
        if (node.args[1] == Symbol("println"))
            return true
        end
    end
    return false
end

#= *******************************************************************************
    EXPECTIONS (Throw, Try-catch, etc).
****************************************************************************** =#

struct TryCatch
    tryBlock
    catchBlock
    finallyBlock
end

isTryCatch(exp) = exp isa Expr && exp.head == :try

function toTryCatch(node)
    tryBlock = node.args[1]
    catchBlock = node.args[3]
    finallyBlock = length(node.args) > 3 ? node.args[4] : false
    tryCatch = TryCatch(tryBlock, catchBlock, finallyBlock) 
    tryCatch
end

struct DomainError <: Exception
    varValue
    message::String
end