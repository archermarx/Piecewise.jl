Base.adjoint(f::Function) = (a...) -> (b...) -> f(a..., b...)

supertype_all(xs::T) where T<:AbstractArray = typeof.(xs) |> foldr'(promote_type)

supertype_all(xs...) = typeof.(xs) |> foldr'(promote_type)

import Base.|, Base.*

Base.:(*)(t1::Union{Type, TypeVar}, t2::Union{Type, TypeVar}) = Tuple{t1, t2}
Base.:(|)(t1::Union{Type, TypeVar}, t2::Union{Type, TypeVar}) = Union{t1, t2}

Base.:(*)(p1::Pair{Symbol, T}, p2::Pair{Symbol, S}) where {T<:DataType, S<:DataType} = NamedTuple{(p1.first, p2.first), p1.second * p2.second}
