struct Line{T}
    a::T
    b::T
end

eltype(::Type{Line{T}}) where T = T

eltype(::Line{T}) where T = T
length(line::Line) = norm(line.a - line.b)

lerp(a, b, t) = a + (b - a)t

const EPSILON = Vec(1e-6, 1e-6, 1e-6)

keys(iter::Line{<:Cube}) = OneTo(length(iter))
firstindex(iter::Line{<:Cube}) = 1
lastindex(iter::Line{<:Cube}) = length(iter)
function getindex(iter::Line{T}, i::Int) where T <: Cube
    N = length(iter)
    @boundscheck 1 <= i <= N || throw(BoundsError(iter, i))

    T(round(lerp(iter.a.qrs, iter.b.qrs, (i - 1)inv(N)) + EPSILON))
end

iterate(iter::Line{<:Cube}) = (iter.a, 2)
function iterate(iter::Line{<:Cube}, i::Int)
    N = length(iter)
    1 <= i <= N || return nothing

    return (@inbounds iter[i], i + 1)
end

# TODO: support GBT by using bresenhem

line(a, b) = norm(a - b) < 1 ? a : Line(a, b)