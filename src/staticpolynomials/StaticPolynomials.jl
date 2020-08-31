export @p_str, StaticPolynomial, differentiate, integrate, degree, coeffs, variable

abstract type AbstractStaticPolynomial{T} <: Function end

struct StaticPolynomial{T<:Number, N} <: AbstractStaticPolynomial{T}
    coeffs::NTuple{N, T}
    var::Symbol
    function StaticPolynomial(coeffs::Tuple, var::Symbol = :x)
        T = supertype_all(coeffs...)
        isempty(coeffs) && return StaticPolynomial{T, 0}()
        N = length(coeffs)
        c = promote(coeffs...) |> NTuple{N, T}
        var = N > 1 ? var : :CONSTANT
        return new{T, N}(c, var)
    end
    StaticPolynomial{T, N}(coeffs::NTuple{N, T}, var::Symbol) where {T<:Number, N}= new{T, N}(coeffs, var)
end

coeffs(p::AbstractStaticPolynomial) = p.coeffs
variable(p::AbstractStaticPolynomial) = p.var

(p::StaticPolynomial)(x) = evalpoly(x, coeffs(p))

StaticPolynomial(coeffs::AbstractVector, var::Symbol = :x) = StaticPolynomial(Tuple(coeffs), var)

# Constructors
function StaticPolynomial(spcoeffs::AbstractVector{T}, exponents::AbstractVector{<:Integer}, var::Symbol = :x) where T<:Number
    coeffs = zeros(T, maximum(exponents) + 1)
    for (c, e) in zip(spcoeffs, exponents)
        coeffs[e + 1] += c
    end
    return StaticPolynomial(coeffs, var)
end

function StaticPolynomial(spcoeffs::Tuple, exponents::Tuple, var::Symbol = :x)
    coeffs = zeros(supertype_all(spcoeffs...), maximum(exponents) + 1)
    for (c, e) in zip(spcoeffs, exponents)
        coeffs[e + 1] += c
    end
    return StaticPolynomial(coeffs, var)
end

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
Base.convert(::Type{T}, x::StaticPolynomial) where T<:StaticPolynomial = T(x)
Base.convert(::Type{T}, x::StaticPolynomial) where T<:Number = T(x)

StaticPolynomial(p::StaticPolynomial) = p 

StaticPolynomial{T}(p::StaticPolynomial{S}) where {T<:Number, S<:Number} = StaticPolynomial(broadcast(T, p.coeffs))

function StaticPolynomial{T, N}(p::StaticPolynomial{S, M}) where {T<:Number, S<:Number, M, N}

    N < M && error("Cannot convert a polynomial of degree $(M-1) to degree $(N-1)")
    coeffs = zeros(T, N)
    for (i, c) in enumerate(p.coeffs)
        coeffs[i] = c
    end
    return StaticPolynomial(coeffs, p.var)
end

function StaticPolynomial{T, N}(p::StaticPolynomial{T, M}) where {T<:Number, N, M}
    N < M && error("Cannot convert a polynomial of degree $(M-1) to degree $(N-1)")
    p1 = StaticPolynomial(zeros(T, N))::StaticPolynomial{T, N}
    return p + p1
end

(::Type{T})(p::StaticPolynomial) where {T<:Number} = 
    degree(p) |> iszero ? T(coeffs(p)[1])::T : throw(InexactError(nameof(T), T, p))

Base.promote_rule(::Type{StaticPolynomial{T, N}}, ::Type{S}) where {T<:Number, S<:Number, N} = 
    StaticPolynomial{promote_type(T, S), N}

Base.promote_rule(::Type{StaticPolynomial{T, N}}, ::Type{StaticPolynomial{S, N}}) where {T<:Number, S<:Number, N} = 
    StaticPolynomial{promote_type(T, S), N}

Base.promote_rule(::Type{StaticPolynomial{T, N}}, ::Type{StaticPolynomial{S, M}}) where {T<:Number, S<:Number, M, N} = 
    StaticPolynomial{promote_type(T, S), max(N, M)}

# define basic arithmetic operations
Base.isapprox(p1::StaticPolynomial{T, N}, p2::StaticPolynomial{S, M}) where {T<:Number, S<:Number, M, N} = 
    p1.var == p2.var && M == N && all([p1[i] == p2[i] for i in 1:M]) 

Base.:(==)(p1::StaticPolynomial, p2::StaticPolynomial) = (p1.coeffs == p2.coeffs && p1.var == p2.var) || (iszero(p1) && iszero(p2)) || (isone(p1) && isone(p2))

function polynomial_arithmetic(operation::Function, p1::StaticPolynomial{T, N}, p2::StaticPolynomial{S, M}) where {T<:Number, S<:Number, M, N}
    R = promote_type(T, S)
    if p1.var == p2.var || p1.var == :CONSTANT || p2.var == :CONSTANT
        return (R(operation(p1[i], p2[i])) for i in 1:max(M, N)) |> collect |> StaticPolynomial
    else
        error("Polynomials must be in terms of the same variable.")
    end
end

Base.:(+)(p1::StaticPolynomial, p2::StaticPolynomial) = polynomial_arithmetic(+, p1, p2)
Base.:(+)(p::StaticPolynomial, n::S) where {S<:Number} = p + StaticPolynomial(n)
Base.:(+)(n::S, p::StaticPolynomial) where {S<:Number} = p + n

Base.:(-)(p1::StaticPolynomial, p2::StaticPolynomial) = polynomial_arithmetic(-, p1, p2)
Base.:(-)(p::StaticPolynomial, n::S) where {S<:Number} = p - StaticPolynomial(n)
Base.:(-)(n::S, p::StaticPolynomial) where {S<:Number} = StaticPolynomial(n) - p

Base.:(/)(p::StaticPolynomial, n::S) where {S<:Number} = StaticPolynomial(coeffs(p)./n)

Base.:(//)(p::StaticPolynomial, n::S) where {S<:Number} = StaticPolynomial(coeffs(p).//n)

function Base.:(^)(p::StaticPolynomial, n::S) where {S<:Integer}
    if n == 1
        return p
    elseif n == 0 
        return one(p)
    else
        return p * p^(n-1)
    end
end

Base.zero(p::StaticPolynomial{T, N}) where {T<:Number, N} = zero(StaticPolynomial{T, N}, p.var)
Base.zero(::Type{StaticPolynomial{T, N}}, var=:x) where {T, N} = StaticPolynomial(zeros(T, N), var)
Base.zero(::Type{StaticPolynomial{T}}) where T = zero(StaticPolynomial{T, 1})
Base.zero(::Type{StaticPolynomial}) = zero(StaticPolynomial{Int})

Base.one(p::StaticPolynomial{T, N}) where {T<:Number, N} = one(StaticPolynomial{T, N}, p.var)
Base.one(::Type{StaticPolynomial{T, N}}, var=:x) where {T, N} = zero(StaticPolynomial{T, N}, var) + 1
Base.one(::Type{StaticPolynomial{T}}) where T = one(StaticPolynomial{T, 1})
Base.one(::Type{StaticPolynomial}) = one(StaticPolynomial{Int})

function Base.:(*)(p1::StaticPolynomial{T, N}, p2::StaticPolynomial{S, M}) where {T<:Number, S<:Number, N, M}
    !(p1.var == p2.var || p1.var == :CONSTANT || p2.var == :CONSTANT) && error("Polynomials must be in terms of the same variable.")
    
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
function differentiate(p::StaticPolynomial, order = 1) 
    if order == 0 || (iszero(p) && length(p) <= 1)
        return p
    elseif length(p) == 1 && !iszero(p)
        return zero(p)
    else
        coeffs = (i * p[i + 1] for i in 1:length(p)-1) |> collect 
        var = length(coeffs) <= 1 ? :CONSTANT : p.var
        p = StaticPolynomial(coeffs, var)
        return differentiate(p, order - 1)
    end
end

function integrate(p::StaticPolynomial; C = 0)
    n = length(p)
    cs = zeros(Float64, n + 1)
    cs[1] = C
    cs[2:end] = [p[i] / i for i in 1:n]
    var = p.var == :CONSTANT ? :x : p.var
    return StaticPolynomial(cs, var)
end

#StaticPolynomial IO stuff
function exponent_string(exponent::Integer, mimetype=MIME"text/plain"())
    if exponent < 0 
        return "-" * exponent_string(-exponent, mimetype)
    elseif exponent > 9
        return exponent_string(exponent ÷ 10, mimetype) * exponent_string(exponent % 10, mimetype)
    else
        if mimetype === MIME"text/latex"()
            return "^{" * string(exponent) * "}"
        else
            return exponent_dict[exponent]
        end
    end
end

const exponent_dict = Dict{Int, String}(
    0 => "⁰",
    1 => "¹",
    2 => "²",
    3 => "³",
    4 => "⁴",
    5 => "⁵",
    6 => "⁶",
    7 => "⁷",
    8 => "⁸",
    9 => "⁹",
)

poly_coeff_string(coeff) = @match coeff begin
    if isone(coeff) end => ""
    ::Complex => "(" * string(coeff) * ")"
    _ => string(coeff) 
end

poly_term_string(coeff, exponent, var, mimetype) = @match exponent begin
    0 => isone(coeff) ? string(coeff) : poly_coeff_string(coeff)
    1 => poly_coeff_string(coeff) * (coeff isa Rational ? "*" : "") * string(var) 
    _ => poly_coeff_string(coeff) * (coeff isa Rational ? "*" : "") * string(var) * exponent_string(exponent, mimetype)
end

signstring(n::Number) = signbit(n) ? "-" : "+"

function Base.show(io::IO, p::StaticPolynomial{T, N}) where {T<:Number, N} 
    print(io, polyname(p), "(")
    print(io, printpoly(p))
    print(io, ")")
end

polyname(p) = string(typeof(p))

function printpoly(p::StaticPolynomial{T, N}, mimetype=MIME"text/plain"()) where {T<:Number, N} 
    str = ""
    if iszero(p)
        str = string(zero(T))
    else
        if T <: Real
            for (i, c) in enumerate(coeffs(p))
                if c != 0
                    str *= isempty(str) ? poly_term_string(c, i - 1, p.var, mimetype) : " " * signstring(c) * " " * poly_term_string(abs(c), i-1, p.var, mimetype)
                end 
            end
        else
            for (i, c) in enumerate(coeffs(p))
                if c != 0
                    str *= isempty(str) ? "" : " + " 
                    str *= poly_term_string(c, i - 1, p.var, mimetype)
                end
            end
        end
    end
    return str
end

Base.show(io::IO, mimetype::MIME"text/plain",  p::StaticPolynomial{T, N}) where {T<:Number, N} = Base.show(io, p)

function Base.show(io::IO, ::MIME"text/latex",  p::StaticPolynomial{T, N}) where {T, N}
    print(io, "\$", polyname(p), "(")
    printpoly(p, MIME"text/latex"())
    print(io, ")\$")
end
