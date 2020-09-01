export OrderedPiecewiseFunction

struct OrderedPiecewiseFunction{N, T<:Real, F<:Function} <: AbstractPiecewise{N}
    functions::Vector{<:Function}
    breakpoints::Vector{T}
    evalfunc::F
    expressions::Vector{String}
    function OrderedPiecewiseFunction{N}(functions::Vector{Function}, breakpoints::Vector{T}, evalfunc::F, expressions::Vector{String}) where {N, T<:Real, F<:Function}
        @assert length(functions) ==  N + 1
        @assert length(breakpoints) == N
        @assert issorted(breakpoints)
        new{N, T, F}(functions, breakpoints, evalfunc, expressions)
    end
end

function OrderedPiecewiseFunction{N}(functions::Vector{Function}, breakpoints::Vector{T}) where {N, T<:Real}
    evalfunc = x -> evalpiecewise(Val(N), functions, breakpoints, x)
    expressions = [quote $f end for f in functions] .|> string
    OrderedPiecewiseFunction{N}(functions, breakpoints, evalfunc, expressions)
end

(p::OrderedPiecewiseFunction{N, T, F})(x) where {N, T, F} = p.evalfunc(x)

function Base.show(io::IO, p::OrderedPiecewiseFunction{N, T, F}) where {N, T, F}
    println(io, polyname(p) * ": ")
    printpiecewise(io, p.expressions, p.breakpoints)
end


