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

functions = [
    x -> 0.0,
    x -> 1.0 - x^2,
    x -> x^2 - 1.0,
    x -> 0.0
]

breakpoints = [-1.0, 0.0, 1.0]

mypiecewise = PiecewisePolynomial{3}(polynomials, breakpoints)
mypiecewise_funcs = OrderedPiecewiseFunction{3}(functions, breakpoints)

differentiated_polys = [zero(StaticPolynomial{Float64, 2}), p"-2.0x", p"2.0x", zero(StaticPolynomial{Float64, 2})]
integrated_polys = [zero(StaticPolynomial{Float64, 4}), p"x - x^3/3", p"x^3/3 - x", zero(StaticPolynomial{Float64, 4})]

mypiecewise_differentiated = PiecewisePolynomial{3}(differentiated_polys, breakpoints)
mypiecewise_integrated = PiecewisePolynomial{3}(integrated_polys, breakpoints)

@test mypiecewise_differentiated == differentiate(mypiecewise)
@test mypiecewise_integrated == integrate(mypiecewise)

@test mypiecewise(1.1) == mypiecewise(-1.1) == mypiecewise(-1) == 0 == mypiecewise_funcs(1.1) == mypiecewise_funcs(-1.1) == mypiecewise(-1)

op1 = Piecewise.@ordered_piecewise begin
    1 => x -> 0
    _ => x -> 1
end

op2 = Piecewise.@ordered_piecewise [x -> 0, x -> 1] [1]

pp1 = Piecewise.@piecewise_polynomial begin
    1 => p"0"
    _ => p"1"
end

pp2 = Piecewise.@piecewise_polynomial [p"0", p"1"] [1]

@test op1(1) == pp1(1) == op2(1) == pp2(1) == 1
@test op1(0.99) == pp1(0.99) == pp2(0.99) == 0

pp3 = Piecewise.@piecewise_polynomial begin
    1 => p"0"
    _ => p"x"
end

@test all(length.(pp3.polynomials) .== 2) && pp3.polynomials isa Vector{StaticPolynomial{Int64, 2}}

op_macro = Piecewise.@ordered_piecewise begin
    -1.0 => x -> 0.0
    0.0 => x -> 1.0 - x^2
    1.0 => x -> x^2- 1.0 - cos(x^2)
    _ => x -> 0.0
end

@test op_macro.expressions == ["x -> 0.0", "x -> 1.0 - x^2", "x -> (x^2 - 1.0) - cos(x^2)", "x -> 0.0"]

# TODO: Write tests for printing

end

nothing
