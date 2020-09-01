
    export PiecewisePolynomial, differentiate, integrate
    
    struct PiecewisePolynomial{N, P<:StaticPolynomial, T<:Real} <: AbstractPiecewise{N}
        polynomials::Vector{P}
        breakpoints::Vector{T}
        function PiecewisePolynomial{N}(polynomials::Vector{P}, breakpoints::Vector{T}) where {N, P<:StaticPolynomial, T<:Real}
            @assert length(polynomials) ==  N + 1
            @assert length(breakpoints) == N
            @assert issorted(breakpoints)
            polys = [p for p in promote(polynomials...)]
            new{N, P, T}(polys, breakpoints)
        end
    end
    
    (p::PiecewisePolynomial{N, P, T})(x) where {N, P, T} = evalpiecewise(Val(N), p.polynomials, p.breakpoints, x)

    differentiate(p::PiecewisePolynomial{N}, order=1) where N = 
        PiecewisePolynomial{N}(differentiate.(p.polynomials, order), p.breakpoints)
    
    integrate(p::PiecewisePolynomial{N}; C=0) where N = 
        PiecewisePolynomial{N}(integrate.(p.polynomials, C=C), p.breakpoints)

    Base.:(==)(p1::PiecewisePolynomial, p2::PiecewisePolynomial) = p1.polynomials == p2.polynomials && p1.breakpoints == p2.breakpoints
