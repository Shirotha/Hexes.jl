struct Cone{T, D}
    center::T
    radius::Int
end

eltype(::Type{<:Cone{T}}) where T = T

eltype(::Cone{T}) where T = T
length(iter::Cone) = ((iter.radius + 2) * (iter.radius + 1)) >> 1

keys(iter::Cone) = (CartesianIndex(r, l) for r in 0:iter.radius, l in 0:r)

function getindex(iter::Cone{T, D}, r::Int, l::Int) where {T, D}
    @boundscheck 0 <= l <= r || throw(BoundsError(iter, (r, l)))

    unit_radius = T(Val{D}())
    unit_rotate = rotl120(unit_radius)

    return iter.center + unit_radius * r + unit_rotate * l
end

iterate(iter::Cone) = (iter.center, (1, 0))
# TODO: this is slow for GBT, use iterative approch instead
function iterate(iter::Cone{T, D}, (r, l)::Tuple{Int, Int}) where {T, D}
    0 <= l <= r <= iter.radius || return nothing

    value = @inbounds iter[r, l]

    if l == r
        r += 1
        l = 0
    else
        l += 1
    end

    return value, (r, l)
end

cone(center, radius, direction) = radius < 1 ? center : Cone{typeof(center), mod(direction - 1, 6) + 1}(center, radius)