using Piecewise
using Test
using Measurements

include("algebraic_datatypes.jl")

@testset "Static polynomials" begin
    include("staticpolynomials.jl")
end

@testset "Piecewise functions" begin

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

end


nothing