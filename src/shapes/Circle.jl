struct Circle{T}
    center::T
    radius::Int
end

eltype(::Type{Circle{T}}) where T = T

eltype(::Circle{T}) where T = T
length(iter::Circle) = 1 + 3(iter.radius + 1)iter.radius

function iterate(iter::Circle{T}) where T
    ring = Ring(iter.center, 1)
    state = (0, T(Val{0}()))
    return iter.center, (ring, state)
end
function iterate(iter::Circle{T}, (ring, state)::Tuple{Ring{T}, Tuple{Int, T}}) where T
    result = iterate(ring, state)

    if isnothing(result)
        ring.radius == iter.radius && return nothing

        ring = Ring(ring.center, ring.radius + 1)
        value, state = iterate(ring)
    else
        value, state = result
    end
    
    return value, (ring, state)
end

circle(center, radius) = radius < 1 ? center : Circle(center, radius)
