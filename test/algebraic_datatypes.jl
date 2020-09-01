@testset "Algebraic datatypes" begin

@test Piecewise.supertype_all(Real[1, 2.0]) == Float64
@test Int * Float64 == Tuple{Int, Float64}
@test Int | Float64 == Union{Int, Float64}

test_union_typevar_1(::Type{T}, ::Type{Float64}) where T = T | Float64
test_union_typevar_2(::Type{Float64}, ::Type{T}) where T = Float64 | T
test_union_typevar_3(::Type{T}, ::Type{S}) where {T, S} = T | S

@test test_union_typevar_1(Int, Float64) == Int | Float64
@test test_union_typevar_2(Float64, Int) == Float64 | Int
@test test_union_typevar_3(Float64, Int) == Float64 | Int

@test (:Num1 => Int) * (:Num2 => Float64) == NamedTuple{(:Num1, :Num2), Int * Float64}
p = (:Num1 => Int) * (:Num2 => Float64)
testp = p((10, 1.0))
@test testp.Num1 === 10
@test testp.Num2 === 1.0
 
end