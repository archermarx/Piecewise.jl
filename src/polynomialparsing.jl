split_polynomial(p_str, var = 'x') = split(p_str, r"(?=[+,-])") .|> parse_term

function parse_term(term, var = 'x')
    #@show term
    stripped = strip(term)
    xind = findfirst(var, stripped)
    isnothing(xind) && return parse_number(stripped), 0
    coeff = try 
        parse_number(stripped[1:xind-1])
    catch ArgumentError
        parse_number(stripped[1:xind-1]*"1")
    end
    xind == length(stripped) && return coeff, 1
    exponent = tryparse(Int, stripped[(xind+2):end])
    isnothing(exponent) && error("Polynomials must have positive integer exponents")
    return coeff, exponent
end
