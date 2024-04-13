include("struct.jl")

function metajulia_eval(parsed, env = initialEnv())
    if (selfEvaluating(parsed))
        parsed
    elseif (isPrintln(parsed))
        evalPrintln(parsed, env)
    elseif isName(parsed)
        evalName(parsed, env)
    elseif isTryCatch(parsed)
        evalTryCatch(toTryCatch(parsed), env)
    elseif isQuoteNode(parsed)
        evalQuoteNode(parsed, env)
    elseif isMacro(parsed)
        evalMacro(toMacro(parsed, env), env)
    elseif isLet(parsed)
        evalLet(toLet(parsed), env)
    elseif isCall(parsed)
        evalCall(toCall(parsed), env)
    elseif isIf(parsed)
        evalIf(toIf(parsed), env)
    elseif isEqual(parsed)
        evalEqual!(toEqual(parsed), env)
    elseif isBlock(parsed)
        evalBlock(toBlock(parsed), env)
    elseif isAnon(parsed)
        toAnon(parsed, env)
    elseif isOr(parsed)
        evalOr(parsed, env)
    elseif isAnd(parsed)
        evalAnd(parsed, env)
    elseif isGlobal(parsed)
        evalSet(toEqual(parsed.args[1]), env)
    else
        error("Unknown expression type -- EVAL: $exp")
    end
end

#= *******************************************************************************
    EXPECTIONS (Throw, Try-catch, etc).
****************************************************************************** =#

function evalTryCatch(node::TryCatch, env)
    try
        evalExprs(node.tryBlock, env)[end]
    catch e
        if (evalExprs(node.catchBlock, env)[end] == false)
            return nothing
        end
    finally
        if (evalExprs(node.finallyBlock, env)[end] == false) 
            return nothing
        end
    end
end

function evalPrintln(node, env)
    
    finalString::String = ""
    
    for arg in node.args[2:end]
        
        if (arg isa Symbol)
            v = evalName(String(arg), env)
            while v isa Variable
                v = v.value
            end
            finalString *= repr(v)
        else
            finalString *= arg
        end
        
    end
    println(finalString)
    
end

function evalSet(node, env)

    if isCall(node.left)
        
        call::Call = toCall(node.left)
        
        if (isBlock(node.init))
            
            fnEnv = Environment(env.frames)
            fnDef::FuncDef = FuncDef(call.fn, call.args, node.init.args, fnEnv)
            defineName!(String(fnDef.name), fnDef, Environment(reverse(env.frames)))
            defineName!(String(fnDef.name), fnDef, fnEnv)
            return fnDef
            
        else
            
            fnEnv = Environment(copy(env.frames))
            fExpr::Fexpr = Fexpr(call.fn, call.args, node.init.args, fnEnv)
            defineName!(String(fExpr.name), fExpr, Environment(reverse(env.frames)))
            defineName!(String(fExpr.name), fExpr, fnEnv)
            return fExpr
            
        end
    else
        value = metajulia_eval(node.init, env)

        if (value isa Fexpr)
            fn::FuncDef = FuncDef(node.left, [], value, env)
            defineName!(node.left, value, Environment(reverse(env.frames)))
            return fn
        end

        defineName!(node.left, value, Environment(reverse(env.frames)))
        value
    end
end

function evalQuoteSet(node, env)

    if isCall(node.left)

        call::Call = toCall(node.left)

        fnEnv = Environment(copy(env.frames))
        fnDef::Fexpr = Fexpr(call.fn, call.args, node.init.args, fnEnv)
        defineName!(String(fnDef.name), fnDef, Environment(reverse(env.frames)))
        defineName!(String(fnDef.name), fnDef, fnEnv)

        fnDef
    else
        value = metajulia_eval(node.init, env)
        defineName!(node.left, value, Environment(reverse(env.frames)))
        value
    end
end


function evalBlock(node::Block, env)
    evalExprs(node.els, env)[end]
end

function evalOr(exp, env)
    metajulia_eval(exp.args[1], env) || metajulia_eval(exp.args[2], env)
end

function evalAnd(exp, env)
    metajulia_eval(exp.args[1], env) && metajulia_eval(exp.args[2], env) 
end

function evalQuoteNode(exp, env)
    if exp isa QuoteNode
        exp.value
    else
        evalQuoteRecurs(exp.args[1], env)
    end
end

function evalQuoteRecurs(exp, env)
    if !(exp isa Expr)
        return exp
    end
    args = []
    for arg in exp.args
        if arg isa Expr && arg.head == :$
            evaluated_arg = metajulia_eval(arg.args[1], env)
        else
            evaluated_arg = evalQuoteRecurs(arg, env)
        end
        push!(args, evaluated_arg)
    end
    Expr(exp.head, args...)
end

function evalMacro(node::Macro, env)
    defineName!(node.name, node, env)  # Define the macro name in the environment
    return node
end

function evalIf(node::If, env)
    if metajulia_eval(node.test, env)
        return metajulia_eval(node.thenBlock, env)
    else
        if isa(node.elseBlock, If)
            return evalIf(node.elseBlock, env)
        elseif node.elseBlock !== nothing
            return metajulia_eval(node.elseBlock, env)
        end
    end
end

function evalEqual!(node::Equal, env)
    
    if isCall(node.left)
        call::Call = toCall(node.left)
        
        fnEnv = Environment(deepcopy(env.frames))
        if (isBlock(node.init))
            fn = FuncDef(call.fn, call.args, node.init.args, fnEnv)
        else #quote equals
            fn = Fexpr(call.fn, call.args, node.init, fnEnv)
        end  
        defineName!(String(fn.name), fn, env)
        defineName!(String(fn.name), fn, fnEnv)
        fn
    elseif !(node.left isa Symbol)
        error("Invalid attribution!")
    else
        value = metajulia_eval(node.init, env)
        defineName!(node.left, value, env)
        value
    end
end

function applyPrimitiveFunction(prim, args)
    if prim in ["+", "-", "*", "/", "^"]
        applyArithmeticFunction(prim, args)
    else
        applyLogicalFunction(prim, args)
    end
end

function applyArithmeticFunction(prim, args)
    if length(args) == 1
        return args[1]
    else
        previousResult = applyArithmeticFunction(prim, args[1:end-1])
        if prim == "+"
            return previousResult + args[end]
        elseif prim == "-"
            return previousResult - args[end]
        elseif prim == "*"
            return previousResult * args[end]
        elseif prim == "/"
            return previousResult / args[end]
        elseif prim == "^"
            return previousResult ^ args[end]
        else
            error("Unsupported OP")
        end
    end
end


function applyLogicalFunction(prim, args)
    if prim == "=="
        args[1] == args[2]
    elseif prim == "<"
        args[1] < args[2]
    elseif prim == ">"
        args[1] > args[2]
    elseif prim == "<="
        args[1] <= args[2]
    elseif prim == ">="
        args[1] >= args[2]
    elseif prim == "!"
        if length(args) != 1
            error("'!' operator expects exactly ONE arg.")
        end
        !args[1]
    end
end

function evalCall(node::Call, env)
    primitive = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "!", "^"]
    fnDef = metajulia_eval(node.fn, env)
    if !(fnDef isa FuncDef) && !(fnDef isa Fexpr)
        argValues = evalExprs(node.args, env)
        
        if node.fn == Symbol("!") && length(argValues) != 1
            error("'!' operator expects exactly ONE arg.")      
        elseif node.fn == :- && length(argValues) == 1
                return -(argValues[1])
        elseif (node.fn isa Symbol && String(node.fn) in primitive)
            applyPrimitiveFunction(String(node.fn), argValues)
        end
    else
        if node.fn == Symbol("throw") && !isempty(node.args)
            errorCall = node.args[1].args[1]
            if haskey(errorHandlers, errorCall)
                errorHandlers[errorCall](node.args[1].args, env)
            else
                error("Unhandled error type: $(errorCall)")
            end
        end
        fnEnv = fnDef.env
        if fnDef isa Fexpr
            argValues = evalQuoteRecurs(node.args, env)
        else
            argValues = evalExprs(node.args, env)
        end
        for i in 1:length(fnDef.params)
            defineName!(fnDef.params[i], argValues[i], fnEnv)
        end
        if fnDef isa Fexpr
            defineName!("eval", FuncDef(Symbol("eval"), [], [], env), fnEnv)
        end
        if fnDef.name == Symbol("eval")
            evalExprs(argValues, fnEnv)[1]
        else
            evalExprs(fnDef.body, fnEnv)[end]
        end
    end
end

function handleDomainError(args, env)
    varValue = evalExprs(args[2], env)[1]
    message = args[3]
    error("DomainError with $(varValue): \n$(message)")
end

errorHandlers = Dict(
    Symbol("DomainError") => handleDomainError,
)

function evalName(name, env)
    
    function lookupInFrames(frames)
        if (isempty(frames))
            error("Unbound variable $name")
        end
        for v in frames[1].bindings
            if v.name == String(name)
                return v.value
            end
        end
        lookupInFrames(frames[2:end])
    end
    lookupInFrames(env.frames)
end

function defineName!(name, value, env)
    localEnv = env.frames[1]
    found = false
    for v in localEnv.bindings
        if v.name == String(name)
            v.value = value
            found = true
        end
    end
    if !found
        append!(localEnv.bindings, [Variable(String(name), value)])
    end
end

function evalLet(node::Let, env)
    
    equals = map((x,y) -> Equal(x, y), node.names, node.inits)
    extendedEnv = augmentEnv!([], [], Environment(copy(env.frames)))
    
    for equal in equals
        evalEqual!(equal, extendedEnv)
    end
    
    evalExprs(node.body, extendedEnv)[end]
end

function evalExprs(exps, env)
    
    if (exps isa Array)
        if (isempty(exps))
            return []
        else
            return [metajulia_eval(exp, env) for exp in exps]
        end
    end
    
    return [metajulia_eval(exps, env)]
end

function augmentEnv!(names, values, env)
    env.frames = append!([Frame(map((x,y) -> Variable(x, y), names, values))], env.frames)
    env
end

function selfEvaluating(exp)
    exp isa Number || exp isa String || exp isa Bool || exp isa LineNumberNode
end

function initialEnv()
    initialFrame = Frame([
    Variable("+", ArithmeticOp("+")),
    Variable("-", ArithmeticOp("-")),
    Variable("*", ArithmeticOp("")),
    Variable("/", ArithmeticOp("/")),
    Variable("^", ArithmeticOp("^")),
    Variable("==", LogicalOp("==")),
    Variable("<", LogicalOp("<")),
    Variable(">", LogicalOp(">")),
    Variable("<=", LogicalOp("<=")),
    Variable(">=", LogicalOp(">=")),
    Variable("!", LogicalOp("!")),
    ])

    env = Environment([initialFrame])
    defineName!("throw", FuncDef(Symbol("throw"), [], [], env), env)
    defineName!("DomainError", FuncDef(Symbol("DomainError"), [], [], env), env)
    metajulia_eval(Meta.parse("sqrt(x) = x^0.5"), env)
    env
end

function repl()
    function getBuffer(buffer)
        input = readline()
        if (buffer != "")
            input = buffer * "\n" * input
        end
        parsed = Meta.parse(input)
        
        if isIncomplete(parsed)
            buffer = input
            print("\t")
            getBuffer("\t" * buffer)
        else
            buffer = ""
            parsed
        end
    end
    
    globalEnv = initialEnv()
    while true
        print(">> ") 
        input = getBuffer("")
        output = metajulia_eval(input, globalEnv)
        if (!isnothing(output))
            display(output)
        end
    end 
end