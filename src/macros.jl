export @ordered_piecewise

function generate_ordered_piecewise(bps, funcs)
    ndiv = length(bps)
    str = "x -> begin\n"
    str *= "if x < $(bps[1])\n"
    str*= "return (" * string(funcs[1]) * ")(x)\n"
    for i in 2:ndiv
        str*= "elseif x < $(bps[i])\n"
        str*= "return (" * string(funcs[i]) * ")(x)\n"
    end
    str*= "else\n"
    str*= "return (" * string(funcs[end]) * ")(x)\n"
    #str*="return"
    str*= "end\nend"
    return str |> Meta.parse
end

function collect_conditions(ex::Expr; var = :x, check = false)
    block = ex.args
    nstatements = length(block) ÷ 2
    conditions = []
    functions = Expr[]
    for (i, line) in enumerate(block)
        if !(line isa LineNumberNode)
            cond, func = split_condition(line)
            check && check_condition(var, cond)
            push!(conditions, cond)
            push!(functions, func)
        end
    end
    return conditions, functions
end

function check_condition(var, condition)
    @assert any(condition.args .|> string .== string(var)) "Conditions must be defined in terms of $(string(var))"
end

function split_condition(ex::Expr)
    @assert ex.args[1] == :(=>) "Piecewise function definition must be performed with :(=>) operator"
    condition = ex.args[2]
    func = ex.args[3]
    return condition, func
end

numbers(condition) = filter(x -> x isa Number, condition.args)

function parse_block(block::Expr)
    breakpoints, functions = collect_conditions(block)
    @assert breakpoints[end] == :_ "Otherwise condition must be denoted by _ (underscore) character"
    @views breakpoints = [b for b in breakpoints[1:end-1]]

    return breakpoints, functions
end

macro ordered_piecewise(block::Expr)
    breakpoints, functions = parse_block(block)
    expr = generate_ordered_piecewise(breakpoints, functions)
    N = length(breakpoints)
    quote
        OrderedPiecewiseFunction{$N}(eval.($(esc(functions))), $(esc(breakpoints)), $(esc(expr)),)
    end
end

macro ordered_piecewise(functions::Expr, breakpoints::Expr)
    fs, bps = functions.args, breakpoints.args
    bps = [b for b in bps]
    expr = generate_ordered_piecewise(bps, fs)
    N = length(bps)
    quote
        OrderedPiecewiseFunction{$N}(eval.($(esc(fs))), $(esc(bps)), $(esc(expr)),)
    end
end

macro piecewise_polynomial(block::Expr)
    breakpoints, polynomials = parse_block(block)
    N = length(breakpoints)
    quote
        PiecewisePolynomial{$N}(eval.($(esc(polynomials))), $(esc(breakpoints)))
    end
end

macro piecewise_polynomial(polynomials::Expr, breakpoints::Expr)
    polys, bps = polynomials.args, breakpoints.args
    bps = [b for b in bps]
    N = length(bps)
    quote
        PiecewisePolynomial{$N}(eval.($(esc(polys))), $(esc(bps)))
    end
end