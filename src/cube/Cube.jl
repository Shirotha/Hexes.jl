struct Cube{T} <: Hex{T}
    qrs::Vec{3, T}

    function Cube{T}(qrs::Vec{3, U}) where {T, U}
        @assert sum(qrs) == 0 "given $qrs"
        return new{T}(qrs)
    end
    function Cube(qrs::Vec{3, T}) where T
        @assert sum(qrs) == 0 "given $qrs"
        return new{T}(qrs)
    end
    function Cube{T}(q, r, s) where T
        @assert q + r + s == 0 "given ($q, $r, $s)"
        return new{T}(Vec{3, T}((q, r, s)))
    end
end

# Properties
getproperty(cube::Cube, order::Symbol) = order === :qrs ? getfield(cube, :qrs) : swiwwel(getfield(cube, :qrs), Val{order}())

# Constructors
Cube(q::Q, r::R, s::S) where {Q, R, S} = Cube{promote_type(Q, R, S)}(q, r, s)

Cube{T}(q, r) where T = Cube{T}(q, r, -(q + r))
Cube(q, r) = Cube(q, r, -(q + r))


Cube{T}(qr::Vec{2}) where T = Cube{T}(qr[1], qr[2])
Cube(qr::Vec{2, T}) where T = Cube(qr[1], qr[2])

Cube{T}(cube::Cube) where T = Cube{T}(cube.qrs)
Cube{T}(cube::Cube, order::Val) where T = Cube{T}(swiwwel(cube.qrs, order))

Cube(cube::Cube{T}) where T = Cube{T}(cube.qrs)
Cube(cube::Cube{T}, order::Val) where T = Cube{T}(swiwwel(cube.qrs, order))

const QRS = (:q, :r, :s)
for i in 1:3, j in (1, -1)
    let first = QRS[i], second = QRS[mod(i - 1 + j, 3) + 1], last = QRS[mod(i - 1 - j, 3) + 1],
            valtype = Val{Symbol(first, second)}

        @eval function Cube{T}(::$valtype, $first, $second) where T
            $last = -($first + $second)
            Cube{T}(q, r, s)
        end
        @eval function Cube(::$valtype, $first, $second)
            $last = -($first + $second)
            Cube(q, r, s)
        end

        @eval function Cube{T}(::$valtype, v::Vec{2}) where T
            $first = v[1]
            $second = v[2]
            $last = -($first + $second)
            Cube{T}(q, r, s)
        end
        @eval function Cube(::$valtype, v::Vec{2, T}) where T
            $first = v[1]
            $second = v[2]
            $last = -($first + $second)
            Cube{T}(q, r, s)
        end
    end
end

Cube{T}(mode::Symbol, args...) where T = Cube{T}(Val{mode}(), args...)
Cube(mode::Symbol, args...) = Cube(Val{mode}(), args...)

# TODO: support additional arguments like size
macro Cube(kwargs...)
    argname(expr::Expr) = expr.args[1]
    argvalue(expr::Expr) = expr.args[2]

    names = argname.(kwargs)
    values = argvalue.(kwargs)

    valtype = Val{Symbol(names...)}

    return quote
        Cube($valtype(), $(values...))
    end
end

# Conversion
convert(::Type{Cube{<:Any}}, cube::Cube{<:Any}) = Cube(cube)
convert(::Type{Cube{T}}, cube::Cube{T}) where T = cube

# Promotion
promote_rule(::Type{Cube{T}}, ::Type{Cube{U}}) where {T, U} = Cube{promote_type(T, U)}

# Comparison
==(lhs::Cube{T}, rhs::Cube{T}) where T = lhs.qrs === rhs.qrs
iszero(cube::Cube) = cube == zero(cube)

# Component Access
@propagate_inbounds getindex(cube::Cube, i) = cube.qrs[i]
@propagate_inbounds setindex(cube::Cube, x, i) = setindex(cube.qrs, x, i)

eltype(::Cube{T}) where T = T

# Arithmetics
+(lhs::Cube, rhs::Cube) = Cube(lhs.qrs + rhs.qrs)

-(lhs::Cube, rhs::Cube) = Cube(lhs.qrs - rhs.qrs)
-(cube::Cube) = Cube(-cube.qrs)

*(lhs::Cube, rhs::Number) = Cube(lhs.qrs * rhs)
*(lhs::Number, rhs::Cube) = Cube(lhs * rhs.qrs)

/(lhs::Cube, rhs::Number) = Cube(lhs.qrs / rhs)
adjoint(cube::Cube) = cube

# Rotation
rotr120(cube::Cube) = Cube(cube, Val{:rsq}())
rotl120(cube::Cube) = Cube(cube, Val{:sqr}())
rot180(cube::Cube) = Cube(-cube.qrs)
rotr60(cube::Cube) = rot180(rotl120(cube))
rotl60(cube::Cube) = rot180(rotr120(cube))

# Rounding
@inline function safe_rounding(f, qrs::Vec{3, <:Any})
    rounded = f(qrs)
    changed = abs(qrs - rounded)
    @inbounds if changed[2] < changed[1] > changed[3]
        r, s = rounded[2], rounded[3]
        return -(r + s), r, s
    elseif changed[2] > changed[3]
        q, s = rounded[1], rounded[3]
        return q, -(q + s), s
    else
        q, r = rounded[1], rounded[2]
        return q, r, -(q + r)
    end
end

floor(cube::Cube{<:Real}) = Cube(safe_rounding(floor, cube.qrs)...)
floor(::Type{T}, cube::Cube{<:Real}) where T = Cube{T}(safe_rounding(floor, cube.qrs)...)
floor(cube::Cube{<:Integer}) = cube
floor(::Type{T}, cube::Cube{<:Integer}) where T = Cube{T}(cube)

ceil(cube::Cube{<:Real}) = Cube(safe_rounding(ceil, cube.qrs)...)
ceil(::Type{T}, cube::Cube{<:Real}) where T = Cube{T}(safe_rounding(ceil, cube.qrs)...)
ceil(cube::Cube{<:Integer}) = cube
ceil(::Type{T}, cube::Cube{<:Integer}) where T = Cube{T}(cube)

round(cube::Cube{<:Real}) = Cube(safe_rounding(round, cube.qrs)...)
round(::Type{T}, cube::Cube{<:Real}) where T = Cube{T}(safe_rounding(round, cube.qrs)...)
round(cube::Cube{<:Integer}) = cube
round(::Type{T}, cube::Cube{<:Integer}) where T = Cube{T}(cube)

# Vector operations
norm(cube::Cube{<:Integer}) = sum(abs(cube.qrs)) >> 1
norm(cube::Cube) = maximum(abs(cube.qrs))
norm(cube::Cube, p) = sum(abs(cube.qrs) ^ p) ^ inv(p)

dot(lhs::Cube, rhs::Cube) = sum(lhs.qrs * rhs.qrs)

# Neighbours
const CUBE_UNITS = [
    Cube(1, -1, 0)
    Cube(1, 0, -1)
    Cube(0, 1, -1)
    Cube(-1, 1, 0)
    Cube(-1, 0, 1)
    Cube(0, -1, 1)
]
Cube{T}(::Val{0}) where T = Cube{T}(0, 0, 0)
Cube{T}(::Val{1}) where T = Cube{T}(1, -1, 0)
Cube{T}(::Val{2}) where T = Cube{T}(1, 0, -1)
Cube{T}(::Val{3}) where T = Cube{T}(0, 1, -1)
Cube{T}(::Val{4}) where T = Cube{T}(-1, 1, 0)
Cube{T}(::Val{5}) where T = Cube{T}(-1, 0, 1)
Cube{T}(::Val{6}) where T = Cube{T}(0, -1, 1)

zero(::Type{Cube{T}}) where T = Cube{T}(Val{0}())
neighbour(cube::Cube{T}, i) where T = cube + Cube{T}(Val{i}())

# 2D Mapping
const CUBE2POSITION1 = Vec{2, Float64}((3/2,       0))
const CUBE2POSITION2 = Vec{2, Float64}((sqrt(3)/2, sqrt(3)))
function cartesian(cube::Cube; size = 1.0)
    qr = convert(Vec{2, Float64}, cube.qr)
    x = sum(qr * CUBE2POSITION1)
    y = sum(qr * CUBE2POSITION2)
    return Vec{2, Float64}((x, y)) * size
end

const POSITION2CUBE1 = Vec{2, Float64}(( 2/3, 0))
const POSITION2CUBE2 = Vec{2, Float64}((-1/3, sqrt(3)/3))

function Cube{T}(::Val{:xy}, xy::Vec{2, <:AbstractFloat}, size=1.0) where T <: AbstractFloat
    invsize = inv(size)
    q = sum(xy * POSITION2CUBE1) * invsize
    r = sum(xy * POSITION2CUBE2) * invsize
    return Cube{T}(q, r)
end
Cube{T}(val::Val{:xy}, xy::Vec{2, <:AbstractFloat}, size=1.0) where T <: Integer = convert(Cube{T}, round(Cube(val, xy, size)))

Cube{T}(val::Val{:xy}, xy::Vec{2, U}, size=1.0) where {T, U} = Cube{T}(val, convert(Vec{2, Float64}, xy), size)
Cube(val::Val{:xy}, xy::Vec{2, T}, size=1.0) where T = Cube{T}(val, xy, size)

Cube{T}(val::Val{:xy}, x, y, size=1.0) where T = Cube{T}(val, Vec(x, y), size)
Cube(val::Val{:xy}, x, y, size=1.0) = Cube(val, Vec(x, y), size)