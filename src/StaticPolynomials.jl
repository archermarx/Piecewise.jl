include("common.jl")
include("polynomialparsing.jl")

export @p_str, StaticPolynomial, differentiate, integrate, degree, coeffs

abstract type AbstractStaticPolynomial{T} <: Function end

struct StaticPolynomial{T<:Number, N} <: AbstractStaticPolynomial{T}
    coeffs::NTuple{N, T}
    function StaticPolynomial(coeffs::Tuple)
        T = supertype_all(coeffs...)
        isempty(coeffs) && return StaticPolynomial{T, 0}()
        N = length(coeffs)
        c = promote(coeffs...) |> NTuple{N, T}
        return new{T, N}(c)
    end
    StaticPolynomial{T, N}(coeffs::NTuple{N, T}) where {T<:Number, N}= new{T, N}(coeffs)
end

coeffs(p::AbstractStaticPolynomial) = p.coeffs

(p::StaticPolynomial)(x) = evalpoly(x, coeffs(p))

StaticPolynomial(coeffs::AbstractVector) = StaticPolynomial(Tuple(coeffs))

# Constructors
function StaticPolynomial(spcoeffs::AbstractVector{T}, exponents) where T<:Number
    coeffs = zeros(T, maximum(exponents) + 1)
    for (c, e) in zip(spcoeffs, exponents)
        coeffs[e + 1] += c
    end
    return StaticPolynomial(coeffs)
end

function StaticPolynomial(spcoeffs::Tuple, exponents)
    coeffs = zeros(supertype_all(spcoeffs...), maximum(exponents) + 1)
    for (c, e) in zip(spcoeffs, exponents)
        coeffs[e + 1] += c
    end
    return StaticPolynomial(coeffs)
end

StaticPolynomial(p_str::String) = parse(StaticPolynomial, p_str)

StaticPolynomial(coeffs...) = StaticPolynomial(coeffs)

# Indexing and utility functions
degree(p::StaticPolynomial{T, N}) where {T<:Number, N} = N - 1
Base.length(p::StaticPolynomial{T, N}) where {T<:Number, N} = N

Base.getindex(p::StaticPolynomial{T, N}, inds...) where {T<:Number, N} = try
        coeffs(p)[inds...]
    catch BoundsError
        zero(T)
    end
    

# Conversion and promotion rules
convert(::Type{T}, x::StaticPolynomial) where T<:StaticPolynomial = T(x)
convert(::Type{T}, x::StaticPolynomial) where T<:Number = T(x)

StaticPolynomial(p::StaticPolynomial) = p 

StaticPolynomial{T}(p::StaticPolynomial{S}) where {T<:Number, S<:Number} = StaticPolynomial(broadcast(T, p.coeffs))

function StaticPolynomial{T, N}(p::StaticPolynomial{T, M}) where {T<:Number, N, M}
    N < M && error("Cannot convert a polynomial of degree $(M-1) to degree $(N-1)")
    p1 = StaticPolynomial(zeros(T, N))::StaticPolynomial{T, N}
    return p + p1
end

function StaticPolynomial{T, N}(p::StaticPolynomial{S, M}) where {T<:Number, S<:Number, N, M}
    
    N < M && error("Cannot convert a polynomial of degree $(M-1) to degree $(N-1)")
    R = promote_type(T, S)
    !(R isa Type{T}) && error("Cannot convert a polynomial of type $(nameof(S)) to type $(nameof(T))")
    
    p1 = StaticPolynomial(zeros(R, N))::StaticPolynomial{T, N}
    return p + p1 
end

(::Type{T})(p::StaticPolynomial) where {T<:Number} = 
    degree(p) |> iszero ? T(coeffs(p)[1])::T : throw(InexactError(nameof(T), T, p))

promote_rule(::Type{StaticPolynomial{T, N}}, ::Type{S}) where {T<:Number, S<:Number, N} = 
    StaticPolynomial{promote_type(T, S), N}

promote_rule(::Type{StaticPolynomial{T, N}}, ::Type{StaticPolynomial{S, N}}) where {T<:Number, S<:Number, N} = 
    StaticPolynomial{promote_type(T, S), N}

promote_rule(::Type{StaticPolynomial{T, N}}, ::Type{StaticPolynomial{S, M}}) where {T<:Number, S<:Number, M, N} = 
    StaticPolynomial{promote_type(T, S), max(N, M)}

# define basic arithmetic operations
Base.isapprox(p1::StaticPolynomial{T, N}, p2::StaticPolynomial{S, M}) where {T<:Number, S<:Number, M, N} = 
    M == N && all([p1[i] == p2[i] for i in 1:M]) 

Base.:(==)(p1::StaticPolynomial, p2::StaticPolynomial) = (p1.coeffs == p2.coeffs) || (iszero(p1) && iszero(p2)) || (isone(p1) && isone(p2))

function polynomial_arithmetic(operation::Function, p1::StaticPolynomial{T, N}, p2::StaticPolynomial{S, M}) where {T<:Number, S<:Number, M, N}
    R = promote_type(T, S)
    return (R(operation(p1[i], p2[i])) for i in 1:max(M, N)) |> collect |> StaticPolynomial
end

Base.:(+)(p1::StaticPolynomial, p2::StaticPolynomial) = polynomial_arithmetic(+, p1, p2)
Base.:(+)(p::StaticPolynomial, n::S) where {S<:Number} = p + StaticPolynomial(n)
Base.:(+)(n::S, p::StaticPolynomial) where {S<:Number} = p + n

Base.:(-)(p1::StaticPolynomial, p2::StaticPolynomial) = polynomial_arithmetic(-, p1, p2)
Base.:(-)(p::StaticPolynomial, n::S) where {S<:Number} = p - StaticPolynomial(n)
Base.:(-)(n::S, p::StaticPolynomial) where {S<:Number} = StaticPolynomial(n) - p

function Base.:(*)(p1::StaticPolynomial{T, N}, p2::StaticPolynomial{S, M}) where {T<:Number, S<:Number, N, M}
    if M > 1 && N > 1
        degree = M + N - 2
        R = promote_type(T, S)
        cs = zeros(R, degree + 1)
        for i in 1:N
            for j in 1:M
                cs[i + j - 1] += p1[i] * p2[j]
            end
        end
        return StaticPolynomial(cs)
    elseif N > 1
        return p1 * coeffs(p2)[1]
    else
        return p2 * coeffs(p1)[1]
    end
end

Base.:(*)(p::StaticPolynomial, n::S) where {S<:Number} = @match n begin
    if iszero(n) end => StaticPolynomial(n)
    if isone(n) end => p
    _ => StaticPolynomial(n .* p.coeffs)
end

Base.:(*)(n::S, p::StaticPolynomial) where {S<:Number} = p * n

Base.iszero(p::StaticPolynomial) = map(iszero, p.coeffs) |> all
Base.isone(p::StaticPolynomial) = isone(p.coeffs[1]) && map(iszero, p.coeffs[2:end]) |> all

# Differentiation and integration
differentiate(p::StaticPolynomial) = iszero(p) ? p : (i * p[i + 1] for i in 1:length(p)-1) |> collect |> StaticPolynomial

function integrate(p::StaticPolynomial; C = 0)
    n = length(p)
    cs = zeros(Float64, n + 1)
    cs[1] = C
    cs[2:end] = [p[i] / i for i in 1:n]
    return StaticPolynomial(cs)
end

#StaticPolynomial IO stuff
exponent_string(exponent::Integer)  = @match exponent begin
    0 => "⁰"
    1 => "¹"
    2 => "²"
    3 => "³"
    4 => "⁴"
    5 => "⁵" 
    6 => "⁶"
    7 => "⁷"
    8 => "⁸"
    9 => "⁹"
    if exponent < 0 end => "⁻" * exponent_string(-exponent)
    _ => exponent_string(exponent ÷ 10) * exponent_string(exponent % 10)
end

poly_coeff_string(coeff) = @match coeff begin
    if isone(coeff) end => ""
    ::Complex => "(" * string(coeff) * ")"
    _ => string(coeff) 
end

poly_term_string(coeff, exponent) = @match exponent begin
    0 => isone(coeff) ? string(coeff) : poly_coeff_string(coeff)
    1 => poly_coeff_string(coeff) * "x" 
    _ => poly_coeff_string(coeff) * "x" * exponent_string(exponent)
end

signstring(n::Number) = signbit(n) ? "-" : "+"

function Base.show(io::IO, p::StaticPolynomial{T, N}) where {T<:Number, N} 
    if iszero(p)
        print(io, "0")
    else
        str = ""
        if T <: Real
            for (i, c) in enumerate(coeffs(p))
                if c != 0
                    str *= isempty(str) ? poly_term_string(c, i - 1) : " " * signstring(c) * " " * poly_term_string(abs(c), i-1)
                end 
            end
        else
            for (i, c) in enumerate(coeffs(p))
                if c != 0
                    str *= isempty(str) ? "" : " + " 
                    str *= poly_term_string(c, i - 1)
                end
            end
        end
        print(io, str)
    end
end

Base.show(io::IO, ::MIME"text/plain",  p::StaticPolynomial{T, N}) where {T<:Number, N} = Base.show(io, p)

function Base.parse(::Type{StaticPolynomial}, p_str) 
    arr = zip(split_polynomial(p_str)...) |> collect
    return StaticPolynomial(arr[1], arr[2])
end

macro p_str(p)
    StaticPolynomial(p)
end