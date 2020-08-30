using Piecewise
using Test

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
end

@testset "Conversion and promotion" begin
    p1 = StaticPolynomial(1, 2, 3)
    p2 = StaticPolynomial(1)
    p3 = StaticPolynomial(1 + 3im)

    @test_throws InexactError Float64(p1)
    @test Int(p2) == 1
    @test Complex(p3) == 1 + 3im
    @test_throws InexactError Int(p3)

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
    @test p"0" == p"0 + 0x + 0x^2"
    @test p"1" == p"1 + x - x + x^2 - x^2"

    @test p"x + 1" * p"x + 2" == p"x^2 + 3x + 2"
    @test p"2x^2 - x - 1" * p"x + 1" == p"2x^3 + x^2 - 2x - 1"
end

@testset "Differentiation and integration" begin
    @test differentiate(p"x + 1") == p"1"
    @test differentiate(p"3x^2 + 2x + 1") == p"6x + 2"
    @test differentiate(p"5x^6 + 4x^3") == p"30x^5 + 12x^2" 

    @test integrate(p"0") == p"0"
    @test integrate(p"0", C = 1) == p"1"
    @test integrate(p"1", C = 1) == p"x + 1"
    @test integrate(p"9x^2 + 3x^5") == p"3x^3 + 0.5x^6"

    @show p"9x^2 + 3x^5"

end

end
