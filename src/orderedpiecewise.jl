export OrderedPiecewiseFunction

struct OrderedPiecewiseFunction{N, T<:Real, F<:Function} <: AbstractPiecewise{N}
    functions::Vector{<:Function}
    breakpoints::Vector{T}
    evalfunc::F
    function OrderedPiecewiseFunction{N}(functions::Vector{Function}, breakpoints::Vector{T}, evalfunc::F) where {N, T<:Real, F<:Function}
        @assert length(functions) ==  N + 1
        @assert length(breakpoints) == N
        @assert issorted(breakpoints)
        new{N, T, F}(functions, breakpoints, evalfunc)
    end
end

function OrderedPiecewiseFunction{N}(functions::Vector{Function}, breakpoints::Vector{T}) where {N, T<:Real}
    f = x -> evalpiecewise(Val(N), functions, breakpoints, x)
    OrderedPiecewiseFunction{N}(functions, breakpoints, f)
end

(p::OrderedPiecewiseFunction{N, T, F})(x) where {N, T, F} = p.evalfunc(x)
