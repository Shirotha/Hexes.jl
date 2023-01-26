include("util.jl")

abstract type GBT{T} <: Hex{T} end

include("GBTInt.jl")
include("GBTIntDigits.jl")

GBT{T}(decimal::Integer) where T = GBTInt{T}(decimal)
GBT(decimal::Integer) = GBTInt(decimal)