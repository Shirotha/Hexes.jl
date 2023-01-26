struct Ring{T}
    center::T
    radius::Int
end

eltype(::Type{Ring{T}}) where T = T

eltype(::Ring{T}) where T = T
length(iter::Ring) = 6iter.radius

iterate(iter::Ring{T}) where T = iterate(iter, (1, T(Val{0}())))
function iterate(iter::Ring{T}, (i, last)::Tuple{Int, T}) where T
    1 <= i <= length(iter) || return nothing

    if i == 1
        value = iter.center + T(Val{1}()) * iter.radius
        return value, (2, value)
    end

    k = mod((i - 2) ÷ iter.radius + 2, 6) + 1
    value = last + T(Val{k}())
    return value, (i + 1, value)
end

ring(center, radius) = radius < 1 ? center : Ring(center, radius)
