
    export PiecewisePolynomial, differentiate, integrate, @piecewise_polynomial
    
    struct PiecewisePolynomial{N, P<:StaticPolynomial, T<:Real} <: AbstractPiecewise{N}
        polynomials::Vector{P}
        breakpoints::Vector{T}
        function PiecewisePolynomial{N}(polynomials::Vector{P}, breakpoints::Vector{T}) where {N, P<:StaticPolynomial, T<:Real}
            @assert length(polynomials) ==  N + 1
            @assert length(breakpoints) == N
            @assert issorted(breakpoints)
            polys = [p for p in promote(polynomials...)]
            R = supertype_all(polynomials)
            new{N, R, T}(polys, breakpoints)
        end
    end
    
    (p::PiecewisePolynomial{N, P, T})(x) where {N, P, T} = evalpiecewise(Val(N), p.polynomials, p.breakpoints, x)

    differentiate(p::PiecewisePolynomial{N}, order=1) where N = 
        PiecewisePolynomial{N}(differentiate.(p.polynomials, order), p.breakpoints)
    
    integrate(p::PiecewisePolynomial{N}; C=0) where N = 
        PiecewisePolynomial{N}(integrate.(p.polynomials, C=C), p.breakpoints)

    Base.:(==)(p1::PiecewisePolynomial, p2::PiecewisePolynomial) = p1.polynomials == p2.polynomials && p1.breakpoints == p2.breakpoints
    Base.isapprox(p1::PiecewisePolynomial, p2::PiecewisePolynomial) = p1.polynomials ≈ p2.polynomials && p1.breakpoints == p2.breakpoints
    Base.:(≈)(p1::PiecewisePolynomial, p2::PiecewisePolynomial) = isapprox(p1, p2)

    Base.:(*)(p1::PiecewisePolynomial{N}, n::Number) where N = PiecewisePolynomial{N}(p1.polynomials .* n, p1.breakpoints)
    Base.:(*)(n::Number, p1::PiecewisePolynomial{N}) where N = p1 * n
    Base.:(+)(p1::PiecewisePolynomial{N}, n::Number) where N = PiecewisePolynomial{N}(p1.polynomials .+ n, p1.breakpoints)
    Base.:(+)(n::Number, p1::PiecewisePolynomial{N}) where N = p1 + n
    Base.:(-)(p1::PiecewisePolynomial{N}, n::Number) where N = PiecewisePolynomial{N}(p1.polynomials .- n, p1.breakpoints)
    Base.:(-)(n::Number, p1::PiecewisePolynomial{N}) where N = -1 * p1 + n
    Base.:(-)(p1::PiecewisePolynomial{N}) where N = -1 * p1

    function Base.show(io::IO, p::PiecewisePolynomial)
        println(io, polyname(p) * ": ")
        printpiecewise(io, printpoly.(p.polynomials), p.breakpoints)
    end
