module Piecewise

    using Measurements

    include("common.jl")
    include(joinpath("staticpolynomials","StaticPolynomials.jl"))
    include(joinpath("staticpolynomials","parsing.jl"))

    include("piecewisepolynomials.jl")
    include("orderedpiecewise.jl")
    include("macros.jl")
    

 end