function parse_number(numstr)
    num = tryparse(Int, numstr)
    return isnothing(num) ? parse(Float64, remove_spaces(numstr)) : num
end

remove_spaces(str) = filter(x -> !isspace(x), str)

Base.adjoint(f::Function) = (a...) -> (b...) -> f(a..., b...)

supertype_all(xs::T) where T<:AbstractArray = typeof.(xs) |> foldr'(promote_type)
    supertype_all(xs...) = typeof.(xs) |> foldr'(promote_type)

    import Base.|, Base.*

Base.:(*)(t1::Type, t2::Type) = Tuple{t1, t2}
Base.:(|)(t1::Type, t2::Type) = Union{t1, t2}
Base.:(|)(t1::TypeVar, t2::Type) = Union{t1, t2}
Base.:(|)(t1::Type, t2::TypeVar) = Union{t1, t2}

Base.:(*)(p1::Pair{Symbol, T}, p2::Pair{Symbol, S}) where {T<:DataType, S<:DataType} = NamedTuple{(p1.first, p2.first), p1.second * p2.second}
