# optional asserts
if !ismissing(get(ENV, "JULIA_NO_ASSERTIONS", missing))
    macro assert(args...) end
end

# swiwwel
const COORD_NAMES = Base.ImmutableDict(
    'x' => 0,
    'y' => 1,
    'z' => 2,
    
    'q' => 0,
    'r' => 1,
    's' => 2
)

map_coord_name(order::Symbol) = Val{getindex.((COORD_NAMES,), Tuple(String(order)))}()
@generated map_coord_name(::Val{order}) where order = map_coord_name(order)

swiwwel(v::Vec, order) = shufflevector(v, map_coord_name(order))

getproperty(v::Vec, order::Symbol) = order === :data ? getfield(v, :data) : swiwwel(v, Val{order}())

# Vec mixed types
promote_rule(::Type{Vec{N, T}}, ::Type{Vec{N, U}}) where {N, T, U} = Vec{N, promote_type(T, U)}
SIMD.Vec(args...) = Vec(promote(args...)...)
for (op, _, _) in SIMD.BINARY_OPS
    @eval @inline function $op(x::Vec{N}, y::Vec{N}) where N
        return $op(promote(x, y)...)
    end
    # NOTE: needs to be more specific than definition in SIMD
    for F in (Float32, Float64)
        @eval @inline function $op(x::Vec{N, T}, y::$F) where {N, T <: SIMD.IntegerTypes}
            return $op(convert(Vec{N, $F}, x), y)
        end
        @eval @inline function $op(x::$F, y::Vec{N, T}) where {N, T <: SIMD.IntegerTypes}
            return $op(x, convert(Vec{N, $F}, y))
        end
    end
end

# repeated function calls
function nest(f, n::Int, x; kws...)
    @assert n >= 1 "given $n"
    result = f(x; kws...)
    for _ in 2:n
        result = f(result; kws...)
    end
    return result
end
function nestwhile(pred, f, x; kws...)
    result = x
    tmp = f(x; kws...)
    while pred(tmp)
        result = tmp
        tmp = f(result; kws...)
    end
    return result
end
function nestlist(f, n::Int, x; kws...)
    @assert n >= 1
    result = fill(f(x; kws...), n)
    for i in 2:n
        result[i] = f(result[i - 1])
    end
    return result
end