export ∧

Base.adjoint(f::Function) = (a...) -> (b...) -> f(a..., b...)

supertype_all(xs::T) where T<:AbstractArray = typeof.(xs) |> foldr'(promote_type)

supertype_all(xs...) = typeof.(xs) |> foldr'(promote_type)

import Base.|, Base.*

Base.:(*)(t1::Union{Type, TypeVar}, t2::Union{Type, TypeVar}) = Tuple{t1, t2}
Base.:(|)(t1::Union{Type, TypeVar}, t2::Union{Type, TypeVar}) = Union{t1, t2}

Base.:(*)(p1::Pair{Symbol, T}, p2::Pair{Symbol, S}) where {T<:DataType, S<:DataType} = NamedTuple{(p1.first, p2.first), p1.second * p2.second}

∧(f::Function, i::Int) = i == 1 ? f : x -> (f ∧ (i-1))(f(x)) 

abstract type AbstractPiecewise{N} <:Function end
    
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

