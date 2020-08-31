module Piecewise
    
    using MLStyle

    include("common.jl")
    include(joinpath("staticpolynomials","StaticPolynomials.jl"))
    include(joinpath("staticpolynomials","polynomialparsing.jl"))
end