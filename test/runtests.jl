using Piecewise
using Test

@testset "Algebraic datatypes tests" begin
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

@testset "StaticPolynomial tests" begin
@testset "Creation and indexing" begin

    p1 = StaticPolynomial((1, 2, 3))
    p2 = StaticPolynomial([1, 2, 3])
    p3 = StaticPolynomial(1, 2, 3)
    @test p1 == p2
    @test p1 == p3
    @test p1 == StaticPolynomial((1, 2.0, 3))
    @test p1 == StaticPolynomial(p1)
    x = 2
    @test p1(x) == 3x^2+ 2x + 1

    @test StaticPolynomial([1, 2, 3, 2], [1, 3, 0, 1]) == StaticPolynomial(3, 3, 0, 2) 
    @test StaticPolynomial((1, 3, 2, 1, 0.1), (3, 1, 0, 7, 1)) == StaticPolynomial(2, 3.1, 0, 1, 0, 0, 0, 1)

    @test p"1 + 3.0x + 10x^2" == StaticPolynomial(1, 3.0, 10)
    @test p"x - x^2" == StaticPolynomial(0, 1, -1)

    @test Piecewise.degree(p1) == 2
    @test Piecewise.degree(StaticPolynomial(0)) == 0

    @test p1[2] == 2
    @test p1[4] == 0
    @test StaticPolynomial(1).var == :CONSTANT

    @test StaticPolynomial{Int, 3}((1,2,3), :x) == p"3x^2 + 2x + 1"

    @test variable(p"(2s+1)(2s-1)") == :s
    @test variable(p"t^2") == :t
end

@testset "Conversion and promotion" begin
    p1 = StaticPolynomial(1, 2, 3)
    p2 = StaticPolynomial(1)
    p3 = StaticPolynomial(1 + 3im)

    @test_throws InexactError Float64(p1)
    @test Int(p2) == 1
    @test Complex(p3) == 1 + 3im
    @test_throws InexactError Int(p3)

    @test Int(p"0") === 0
    @test Float64(p"2.0") === 2.0

    @test StaticPolynomial{Float64}(p"1 + 2x") === p"1.0 + 2.0x"
    @test StaticPolynomial{Float64, 3}(p"1 + 2x") === p"1.0 + 2.0x + 0.0x^2"
    @test StaticPolynomial{Float64, 3}(p"1 + 2x") |> length == 3
    
    @test promote_rule(StaticPolynomial{Int64, 2}, Float64) == StaticPolynomial{Float64, 2}
    @test promote_rule(StaticPolynomial{Int64, 2}, StaticPolynomial{Float64, 1}) == StaticPolynomial{Float64, 2}
    @test promote_rule(StaticPolynomial{Int64, 2}, StaticPolynomial{Float64, 2}) == StaticPolynomial{Float64, 2}

    @test promote(p"1 + 0*x", p"3 + 2.0x", p"1//2 + x^3") === (p"1.0 + 0.0x^3", p"3.0 + 2.0x + 0.0x^3", p"0.5 + x^3")


end

@testset "Arithmetic operations" begin
    p1 = StaticPolynomial(1,2,3)
    p2 = StaticPolynomial(4,5,6)
    p3 = StaticPolynomial(1.0, 2.1, 3.6)
    p4 = StaticPolynomial(1.0, 3.2)
    @test p1 + p2 == StaticPolynomial(5, 7, 9)
    @test p2 + p3 == StaticPolynomial(5.0, 7.1, 9.6)
    @test p1 + p4 == StaticPolynomial(2.0, 5.2, 3.0)

    @test p2 + 1 == StaticPolynomial(5, 5, 6)
    @test 1 + p2 == StaticPolynomial(5, 5, 6)
    @test p2 + 1.0 == StaticPolynomial(5.0, 5.0, 6.0)
    @test p3 + 1 == StaticPolynomial(2.0, 2.1, 3.6)

    @test p2 - p1 == StaticPolynomial(3, 3, 3)
    @test p2 - 1.1 ≈ StaticPolynomial(2.9, 5.0, 6.0)
    @test 4.0 - p2 ≈ StaticPolynomial(0.0, -5.0, -6.0)

    @test p"x + 1" * 2 == p"2x + 2"
    @test p"3x^2 - x + 1" * 0 |> iszero
    @test p"3x^2 - x + 1" * 0 + 1 |> isone
    @test 0 * p"x + 1" |> iszero
    @test 1 + p"0" |> isone
    @test p"1" * p"x - 2x^2" == p"x - 2x^2"
    @test p"0" * p"x - 2x^2" |> iszero
    @test p"0" == p"0 + 0*x + 0*x^2"
    @test p"1" == p"1 + x - x + x^2 - x^2"

    @test zero(p"1 + 2x") == p"0 + 0*x"
    @test zero(p"2+3r").var == :r 
    @test iszero(zero(p"1 + 2x"))

    @test p"0" !== p"0 + 0*x"
    @test zero(StaticPolynomial) === p"0"
    @test zero(StaticPolynomial{Int, 2}) === p"0 + 0*x"
    @test zero(StaticPolynomial{Int, 2}) !== p"0"

    @test one(p"1 + 2x") == p"1 + 0*x"
    @test isone(one(p"1 + 2x"))
    @test one(StaticPolynomial) === p"1"
    @test one(StaticPolynomial{Float64}) === p"1.0"
    @test one(StaticPolynomial{Float64, 2}) == p"1.0 + 0.0x"


    @test p"x + 1" * p"x + 2" == p"x^2 + 3x + 2"
    @test p"2x^2 - x - 1" * p"x + 1" == p"2x^3 + x^2 - 2x - 1"

    @test p"(x+1)"^2 == p"x^2 + 2x + 1"

    @test p"(s+1)^2" == p"s^2 + 2s + 1" == p"(s+1)"^2 == p"s+1"^2

    @test p"1 + 2x"^0 === p"1 + 0*x"

    @test p"x" * p"x + 1" === p"x(x+1)" === p"x + 1" * p"x" === p"x^2 + x"
end

@testset "Differentiation and integration" begin
    @test differentiate(p"x + 1") == p"1"
    @test differentiate(p"3p^2 + 2p + 1") == p"6p + 2"
    @test differentiate(p"5x^6 + 4x^3") == p"30x^5 + 12x^2" 

    @test differentiate(p"1") === p"0"
    @test differentiate(p"1 + 2x", 2) === p"0"

    @test length(differentiate(StaticPolynomial(0.0, 0.0, 0.0))) == 2

    @test integrate(p"0") == p"0"
    @test integrate(p"0", C = 1) == p"1"
    @test integrate(p"1", C = 1) == p"x + 1"
    @test integrate(p"9p") == p"4.5p^2"
    @test integrate(p"9x^2 + 3x^5") == p"3x^3 + 0.5x^6"

end

@testset "Advanced parsing" begin
    @test p"(3x + 2)" == p"3x + 2"
    @test p"(x + 1)(x - 1)" == p"x + 1" * p"x - 1" == p"x^2 - 1" == p"(x+1)(x-1)"
    @test p"(3x + 2) + (4x + 3)" == p"3x + 2" + p"4x + 3"
    @test p"(3x + 2)/4" == p"3x + 2"/4
    @test p"(2x + 3)//4" == p"2x + 3"//4 == p"1//2*x + 3//4"
    @test p"(x+1)^2" == p"(x+1)"^2 == p"x^2 + 2x + 1"
    @test p"x + (x + (x + (x + (x + (x)))))" == p"6x" == p"6*x"
    
    @test p"x/6" == p"x"/6

    @test p"x//6" == p"x"//6

    wrongvar_error = ErrorException("Polynomials must be in terms of the same variable.")
    @test_throws wrongvar_error (p"q + 2q" + p"x + 2x")
    @test_throws wrongvar_error (p"q + 2q" - p"x + 2x")
    @test_throws wrongvar_error (p"q + 2q" * p"x + 2x")

    @test Piecewise.parse_term(1) == (1, 0, nothing)
    @test Piecewise.parse_term(1//2) == (1//2, 0, nothing)
    @test tryparse(StaticPolynomial, "10p + 2q") === nothing

end

@testset "Printing" begin
    p_str = "2x + x^2"
    @test Piecewise.printpoly(parse(StaticPolynomial, p_str), MIME"text/latex"()) == "2x + x^{2}"
    @test Piecewise.printpoly(parse(StaticPolynomial, p_str)) == "2x + x²"
    @test Piecewise.printpoly(parse(StaticPolynomial, "x^13")) == "x¹³"
end

end

#@testset "Piecewise polynomials" begin
polynomials = [
    StaticPolynomial(0.0, 0.0, 0.0), 
    p"1.0-x^2",
    p"x^2 - 1.0",
    StaticPolynomial(0.0, 0.0, 0.0),
]

breakpoints = [-1.0, 0.0, 1.0]

mypiecewise = PiecewisePolynomial{3}(polynomials, breakpoints)

differentiated_polys = [zero(StaticPolynomial{Float64, 2}), p"-2.0x", p"2.0x", zero(StaticPolynomial{Float64, 2})]
integrated_polys = [zero(StaticPolynomial{Float64, 4}), p"x - x^3/3", p"x^3/3 - x", zero(StaticPolynomial{Float64, 4})]

mypiecewise_differentiated = PiecewisePolynomial{3}(differentiated_polys, breakpoints)
mypiecewise_integrated = PiecewisePolynomial{3}(integrated_polys, breakpoints)

@test mypiecewise_differentiated == differentiate(mypiecewise)
@test mypiecewise_integrated == integrate(mypiecewise)


nothing