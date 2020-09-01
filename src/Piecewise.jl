module Piecewise

    using Measurements

    include("common.jl")
    include(joinpath("staticpolynomials","StaticPolynomials.jl"))
    include(joinpath("staticpolynomials","parsing.jl"))

    export PiecewisePolynomial, OrderedPiecewiseFunction, differentiate, integrate

    abstract type AbstractPiecewise{N} <:Function end

    struct PiecewisePolynomial{N, P<:StaticPolynomial, T<:Real} <: AbstractPiecewise{N}
        polynomials::Vector{P}
        breakpoints::Vector{T}
        function PiecewisePolynomial{N}(polynomials::Vector{P}, breakpoints::Vector{T}) where {N, P<:StaticPolynomial, T<:Real}
            @assert length(polynomials) ==  N + 1
            @assert length(breakpoints) == N
            @assert issorted(breakpoints)
            new{N, P, T}(polynomials, breakpoints)
        end
    end
    
    (p::PiecewisePolynomial{N, P, T})(x) where {N, P, T} = evalpiecewise(Val(N), p.polynomials, p.breakpoints, x)

    differentiate(p::PiecewisePolynomial{N}, order=1) where N = 
        PiecewisePolynomial{N}(differentiate.(p.polynomials, order), p.breakpoints)
    
    integrate(p::PiecewisePolynomial{N}; C=0) where N = 
        PiecewisePolynomial{N}(integrate.(p.polynomials, C=C), p.breakpoints)

    Base.:(==)(p1::PiecewisePolynomial, p2::PiecewisePolynomial) = p1.polynomials == p2.polynomials && p1.breakpoints == p2.breakpoints

    struct OrderedPiecewiseFunction{N, T<:Real} <: AbstractPiecewise{N}
        functions::Vector{<:Function}
        breakpoints::Vector{T}
        function OrderedPiecewiseFunction{N}(functions::Vector{Function}, breakpoints::Vector{T}) where {N, T<:Real}
            @assert length(functions) ==  N + 1
            @assert length(breakpoints) == N
            @assert issorted(breakpoints)
            new{N, T}(functions, breakpoints)
        end
    end

    (p::OrderedPiecewiseFunction{N, T})(x) where {N, T} = evalpiecewise(Val(N), p.functions, p.breakpoints, x)

    function evalpiecewise(::Val{N}, functions, breakpoints, x) where N
        if @generated
            generator = (:(if x < breakpoints[$k]
                return functions[$k](x) 
            end) for k in 1:N)
            quote
                @inbounds begin
                    $(generator...)
                    return functions[$(N+1)](x)
                end
            end
        else
            ind = searchsortedfirst(breakpoints, x)
            return functions[ind](x)
        end
    end

 end