## TODO: Implement support for complex numbers in @p_str macro


remove_spaces(str) = filter(x -> !isspace(x), str)

const operator_lookup = Dict(
    :* => *,
    :/ => /,
    :+ => +,
    :- => -,
    :// => //,
    :^ => ^
)

function parse_term(tree::Expr)
    operator = tree.args[1]
    if operator in (:(*), :(/), :(//)) 
        @views leftovers = tree.args[2:end]
        ind = findfirst(x -> !(x isa Number), leftovers) + 1
        coeff, exponent, var = parse_term(tree.args[ind])
        
        deleteat!(tree.args, ind)
        @assert all(x -> x isa Number, tree.args[2:end])

        coeff *= tree.args[2]
        coeff = operator_lookup[operator](1, coeff)

    elseif tree.args[1] == :(+)
        return parse_term(tree.args[2])
    elseif tree.args[1] == :(-)
        coeff, exponent, var = parse_term(tree.args[2])
        coeff = -1*coeff
    else
        coeff = 1
        exponent = tree.args[3]
        @assert exponent > 0
        var = tree.args[2]::Symbol
    end

    return coeff, exponent, var
end

parse_term(term::Symbol) = (1, 1, term)
parse_term(term::Number) = (term, 0, nothing)

split_polynomial(p_str) = 
    split(p_str, r"(?=[+,-])") .|> remove_spaces .|> Meta.parse .|> parse_term

function parse_unnested_poly(p_str)
    arr = zip(split_polynomial(p_str)...) |> collect
    symbols = arr[3]
    #@show symbols
    if length(symbols) == 1
        var = symbols[1]
        var = isnothing(var) ? :CONSTANT : var
    else
        ind = findfirst(!isnothing, symbols)
        var = isnothing(ind) ? :CONSTANT : symbols[ind]
    end
    #@show var
    @assert all(x -> isnothing(x) || x == var, symbols)
    var == :im && error("Cannot parse complex polynomial")
    return StaticPolynomial(arr[1], arr[2], var)
end

function tryparse_unnested_poly(p_str)
    try
        return parse_unnested_poly(p_str)
    catch e
        return nothing
    end
end

function Base.parse(::Type{StaticPolynomial}, p_str)   
    p = tryparse_unnested_poly(p_str)
    if isnothing(p)
        tree = Meta.parse(p_str)
        new_p_str = string(tree)
        p = tryparse_unnested_poly(new_p_str)
    else
        return p
    end
    
    if isnothing(p)
        a1 = tree.args[1]
        for (i, arg) in enumerate(tree.args)
            if arg isa Expr || arg == :x
                a = parse(StaticPolynomial, string(arg))
                if !isnothing(a)
                    tree.args[i] = a
                end
            end
        end
        
        if tree.args[1] isa StaticPolynomial
            pushfirst!(tree.args, :(*))
        end
        op = tree.args[1]
        rest = tree.args[2:end]

        for (i, a) in enumerate(rest)
            if a == :im
                rest[i] = Complex(false, true)
            end
        end
        return StaticPolynomial(operator_lookup[op](rest...))
    else
        return p
    end
end

function Base.tryparse(::Type{StaticPolynomial}, p_str)
    try parse(StaticPolynomial, p_str)
    catch e
        return nothing
    end
end

StaticPolynomial(p_str::String) = parse(StaticPolynomial, p_str)

macro p_str(p)
    StaticPolynomial(p)
end