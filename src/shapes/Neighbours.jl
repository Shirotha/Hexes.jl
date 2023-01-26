struct Neighbours{T}
    center::T
end

eltype(::Type{Neighbours{T}}) where T = T
length(::Type{<:Neighbours}) = 6

eltype(::Neighbours{T}) where T = T
length(::Neighbours) = 6

iterate(iter::Neighbours) = (neighbour(iter.center, 1), 2)
iterate(iter::Neighbours, i::Int) = 1 <= i <= 6 ? (neighbour(iter.center, i), i + 1) : nothing

neighbours(hex::Hex) = Neighbours(hex)