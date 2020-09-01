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
@btime piecewise_hardcoded(x) setup=(x=4*rand()-2) 
println("Generated piecewise polynomial")
@btime $mypiecewise(x) setup=(x=4*rand()-2) 

myderivative = differentiate(mypiecewise)

println("Generated derivative")
@btime $myderivative(x) setup=(x=4*rand()-2) 

functions = [
    x -> 0.0,
    x -> 1.0 - x^2,
    x -> x^2 - 1.0,
    x -> 0.0    
]

mypiecewisefunction = Piecewise.OrderedPiecewiseFunction{3}(functions, breakpoints)

println("Generated piecewise function")
@btime $mypiecewisefunction(x) setup=(x=4*rand()-2) 

op_macro = Piecewise.@ordered_piecewise begin
    -1.0 => x -> 0.0
    0.0 => x -> 1.0 - x^2
    1.0 => x -> x^2- 1.0
    _ => x -> 0.0
end

pp_macro = Piecewise.@piecewise_polynomial begin
    -1.0 => zero(StaticPolynomial{Float64, 3})
    0.0 => p"1.0 - x^2"
    1.0 => p"x^2 - 1.0"
    _ => zero(StaticPolynomial{Float64, 3})
end

pp_deriv = differentiate(pp_macro)
pp_integ = integrate(pp_macro)

println("Macro ordered piecewise")
@btime $op_macro(x) setup=(x=4*rand()-2)

println("Macro piecewise polynomial")
@btime $pp_macro(x) setup=(x=4*rand()-2)

println("Derivative of macro piecewise polynomial")
@btime $pp_deriv(x) setup=(x=4*rand()-2)

println("Integral of macro piecewise polynomial")
@btime $pp_integ(x) setup=(x=4*rand()-2)