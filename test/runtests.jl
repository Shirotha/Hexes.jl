using Test, SIMD

include("../src/Hexes.jl")
using .Hexes

@testset verbose=true "Hexes" begin

    include("cube/Cube.jl")

    include("gbt/GBTInt.jl")

end