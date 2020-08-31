using BenchmarkTools
using Piecewise

function piecewise_hardcoded(x)
    if x < -1
        return 0.0
    elseif x < 0
        return 1.0 - x^2
    elseif x < 1
        return x^2 -1.0
    else
        return 0.0
    end
end

polynomials = [
    StaticPolynomial(0.0, 0.0, 0.0), 
    p"1.0-x^2",
    p"x^2 - 1.0",
    StaticPolynomial(0.0, 0.0, 0.0),
]

breakpoints = [-1.0, 0.0, 1.0]

mypiecewise = PiecewisePolynomial{3}(polynomials, breakpoints)

println("Hardcoded piecewise polynomial")
@btime piecewise_hardcoded(x) setup=(x=4*rand()-2) samples=100_000
println("Generated piecewise polynomial")
@btime $mypiecewise(x) setup=(x=4*rand()-2) samples=100_000

myderivative = differentiate(mypiecewise)

println("Generated derivative")
@btime $myderivative(x) setup=(x=4*rand()-2) samples=100_000

functions = [
    x -> 0.0,
    x -> 1.0 - x^2,
    x -> x^2 - 1.0,
    x -> 0.0    
]

mypiecewisefunction = Piecewise.OrderedPiecewiseFunction{3}(functions, breakpoints)

println("Generated piecewise function")
@btime $mypiecewisefunction(x) setup=(x=4*rand()-2) samples=100_000
