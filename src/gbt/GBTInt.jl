function validatedigits(ds::T) where T <: Unsigned
    N = maxdigits(T)
    mask = T(7) << 3(N - 1)
    zero = T(0)
    for _ in 1:N
        digit = ds & mask
        digit == mask && continue

        digit == zero && return false

        return true
    end
    return true
end
validatedigits(ds::Signed) = validatedigits(unsigned(ds))

struct GBTInt{T <: Unsigned, U <: Integer} <: GBT{U}
    digits::T

    function GBTInt{T, U}(::typeof(digits), ds) where {T <: Unsigned, U <: Integer}
        @assert validatedigits(ds) "0b$(bitstring(ds))"
        return new{T, U}(ds)
    end
end

# Constructors
function GBTInt{T, U}(dds::Tuple) where {T <: Unsigned, U <: Integer}
    @assert all(d -> 0 <= d <= 6, dds)

    ds = typemax(T)
    Nmax = findlast(>(0), dds)
    isnothing(Nmax) && return GBTInt{T, U}(digits, ds)
    
    Nmin = min(maxdigits(T), Nmax)
    seven = T(7)
    mask = seven
    offset = 0
    for i in 1:Nmin
        diff = T(@inbounds dds[i]) << offset
        ds ⊻= ~diff & mask

        mask <<= 3
        offset += 3
    end
    return GBTInt{T, U}(digits, ds)
end

function GBTInt(::typeof(signed), dds::NTuple{N, <:Integer}) where N
    T = nextunsignedtype(3N)
    U = decimaltype_signed(T)
    return GBTInt{T, U}(dds)
end
function GBTInt(::typeof(unsigned), dds::NTuple{N, <:Integer}) where N
    T = nextunsignedtype(3N)
    U = decimaltype_unsigned(T)
    return GBTInt{T, U}(dds)
end
GBTInt(::Type{T}, dds::Tuple) where T = GBTInt(T, promote(dds...))
GBTInt(dds::Tuple) = GBTInt(signed, promote(dds...))

function GBTInt{T}(::Type{X}, dds::Tuple) where {T, X}
    U = decimaltype_signed(T)
    return GBTInt{T, U}(X, promote(dds...))
end
function GBTInt{T}(dds::Tuple) where T
    U = decimaltype_signed(T)
    return GBTInt{T, U}(promote(dds...))
end

function GBTInt{T, U}(decimal::Integer) where {T <: Unsigned, U <: Integer}
    @assert decimal >= 0

    ds = typemax(T)
    N = maxdigits(T)
    seven = T(7)
    mask = seven
    offset = 0

    for _ in 1:N
        iszero(decimal) && break

        decimal, digit = divrem(decimal, seven)

        diff = trunc(T, digit) << offset
        ds ⊻= ~diff & mask

        mask <<= 3
        offset += 3
    end

    return GBTInt{T, U}(digits, ds)
end
function GBTInt{T}(decimal::Unsigned) where T
    U = decimaltype_unsigned(T)
    return GBTInt{T, U}(decimal)
end
function GBTInt{T}(decimal::Signed) where T
    U = decimaltype_signed(T)
    return GBTInt{T, U}(decimal)
end
function GBTInt(decimal::U) where U <: Unsigned
    T = digitstype(U)
    return GBTInt{T, U}(decimal)
end
function GBTInt(decimal::U) where U <: Signed
    T = digitstype(U)
    return GBTInt{T, U}(decimal)
end

GBTInt{T, U}(::Val{0}) where {T, U} = GBTInt{T, U}(digits, typemax(T))
GBTInt{T, U}(::Val{1}) where {T, U} = GBTInt{T, U}(digits, ~T(6))
GBTInt{T, U}(::Val{2}) where {T, U} = GBTInt{T, U}(digits, ~T(5))
GBTInt{T, U}(::Val{3}) where {T, U} = GBTInt{T, U}(digits, ~T(4))
GBTInt{T, U}(::Val{4}) where {T, U} = GBTInt{T, U}(digits, ~T(3))
GBTInt{T, U}(::Val{5}) where {T, U} = GBTInt{T, U}(digits, ~T(2))
GBTInt{T, U}(::Val{6}) where {T, U} = GBTInt{T, U}(digits, ~T(1))
GBTInt{T, U}(::Val{X}) where {T, U, X} = GBTInt{T, U}(X)

GBTInt{T}(::Val{X}) where {T, X} = GBTInt{T, decimaltype_signed(T)}(Val{X}())
GBTInt{T}(::typeof(signed), ::Val{X}) where {T, X} = GBTInt{T, decimaltype_signed(T)}(Val{X}())
GBTInt{T}(::typeof(unsigned), ::Val{X}) where {T, X} = GBTInt{T, decimaltype_unsigned(T)}(Val{X}())

function GBTInt(::Val{X}) where {X}
    T = digitstype(X)
    U = decimaltype_signed(T)
    return GBTInt{T, U}(Val{X}())
end
GBTInt(::Type{T}, ::Val{X}) where {T, X} = GBTInt{digitstype(X)}(T, Val{X}())

function GBTInt{T, U}(gbt::GBTInt{V}) where {T, U, V}
    sizeT = 8sizeof(T)
    sizeV = 8sizeof(V)
    if sizeT == sizeV
        return GBTInt{T, U}(digits, gbt.digits)
    elseif sizeT < sizeV
        return GBTInt{T, U}(digits, unsafe_trunc(T, gbt.digits))
    else
        ds = typemax(T)
        mask = typemax(V)
        diff = mask ⊻ gbt.digits
        ds ⊻= diff & mask
        return GBTInt{T, U}(digits, ds)
    end
end
function GBTInt{T}(gbt::GBTInt{V, U}) where {T, V, U}
    if U <: Unsigned
        X = decimaltype_unsigned(T)
    else
        X = decimaltype_signed(T)
    end
    return GBTInt{T, X}(gbt)
end
GBTInt(gbt::GBTInt{T, U}) where {T, U} = GBTInt{T, U}(digits, gbt.digits)

const SGBTInt8 = GBTInt{UInt8, Int8}
const SGBTInt16 = GBTInt{UInt16, Int16}
const SGBTInt32 = GBTInt{UInt32, Int32}
const SGBTInt64 = GBTInt{UInt64, Int64}
const SGBTInt128 = GBTInt{UInt128, Int128}
const SGBTInt = GBTInt{UInt, Int}

const UGBTInt8 = GBTInt{UInt8, UInt8}
const UGBTInt16 = GBTInt{UInt16, UInt16}
const UGBTInt32 = GBTInt{UInt32, UInt32}
const UGBTInt64 = GBTInt{UInt64, UInt64}
const UGBTInt128 = GBTInt{UInt128, UInt128}
const UGBTInt = GBTInt{UInt, UInt}

zero(::Type{GBTInt{T, U}}) where {T, U} = GBTInt{T, U}(digits, typemax(T))

macro gbt_str(str)
    dds = reverse(Int.(Tuple(str)) .- 48)
    gbt = GBTInt(dds)
    T = typeof(gbt)
    return quote
        $T(digits, $(gbt.digits))
    end
end
macro ugbt_str(str)
    dds = reverse(Int.(Tuple(str)) .- 48)
    gbt = GBTInt(unsigned, dds)
    T = typeof(gbt)
    return quote
        $T(digits, $(gbt.digits))
    end
end
macro sgbt_str(str)
    dds = reverse(Int.(Tuple(str)) .- 48)
    gbt = GBTInt(signed, dds)
    T = typeof(gbt)
    return quote
        $T(digits, $(gbt.digits))
    end
end

# Promotion
function promote_rule(::Type{GBTInt{T1, U1}}, ::Type{GBTInt{T2, U2}}) where {T1, T2, U1, U2} 
    T = promote_type(T1, T2)
    if T <: Unsigned
        X = decimaltype_unsigned(T)
    else
        X = decimaltype_signed(T)
    end
    U = promote_type(U1, U2, X)
    return GBTInt{T, U} 
end
# Conversion
convert(::Type{T}, gbt::GBTInt) where T <: GBTInt = T(gbt)
convert(::Type{T}, gbt::T) where T <: GBTInt = gbt

# Comparision
function maxdigits(gbt::GBTInt{T}) where T
    result = 0
    N = maxdigits(T)

    mask = T(7)
    digits = gbt.digits
    for i in 1:N
        digit = digits & mask

        digit == mask && return result

        digits >>= 3
        result += 1
    end
    return result
end

iszero(gbt::GBTInt{T}) where T = gbt.digits == typemax(T)
eltype(::GBTInt{T, U}) where {T, U} = U
eltype(::Type{GBTInt{T, U}}) where {T, U} = U

function ==(lhs::GBTInt, rhs::GBTInt)
    N1 = maxdigits(lhs)
    N2 = maxdigits(rhs)

    N1 != N2 && return false
    
    mask = (UIntMax(1) << 3N1) - 1
    
    return (lhs.digits & mask) == (rhs.digits & mask)
end

function <(lhs::GBTInt, rhs::GBTInt)
    N1 = maxdigits(lhs)
    N2 = maxdigits(rhs)

    N1 > N2 && return false
    N1 < N2 && return true

    mask = (UIntMax(1) << 3N1) - 1
    
    return (lhs.digits & mask) < (rhs.digits & mask)
end

# Decimal Conversion
function getindex(gbt::GBTInt{T, U}) where {T, U}
    result = zero(U)
    N = maxdigits(T)
    
    mask = T(7)
    digits = gbt.digits
    for i in 1:N
        digit = digits & mask;
        digit == mask && return result

        digits >>= 3
        if i == 1
            result += trunc(U, digit)
        else
            result += trunc(U, digit * SEVENS[i - 1])
        end
    end
    return result
end

# Arithmetics
const SUM = [
    0 1 2 3 4 5 6;
    1 3 5 2 0 6 4;
    2 5 4 6 3 0 1;
    3 2 6 5 1 4 0;
    4 0 3 1 6 2 5;
    5 6 0 4 2 1 3;
    6 4 1 0 5 3 2
]
const CARRY = [
    0 0 0 0 0 0 0;
    0 6 1 0 0 0 6;
    0 1 1 2 0 0 0;
    0 0 2 2 3 0 0;
    0 0 0 3 3 4 0;
    0 0 0 0 4 4 5;
    0 6 0 0 0 5 5;
]
# FIXME: broken after using last_nonzero
function +(lhs::GBTInt{T, U}, rhs::GBTInt{T, U}) where {T, U}
    ds = T(0)
    N = maxdigits(T)

    offset = 0
    zero = T(0)
    one = T(1)
    mask = T(7)
    carry = 0
    last_nonzero = 0
    for i in 1:N
        dl = lhs.digits & mask
        dr = rhs.digits & mask
        
        if dl == mask
            dl = zero
            if dr == mask
                iszero(carry) && break

                last_nonzero = i
                diff = T(carry) << offset
                ds |= diff
                break
            end
        elseif dr == mask
            dr = zero
        end

        dl = (dl >> offset) + one
        dr = (dr >> offset) + one

        dsum = @inbounds SUM[dl, dr] + one
        dcarry = carry + one

        digit = @inbounds SUM[dsum, dcarry]
        carry = @inbounds SUM[CARRY[dl, dr] + 1, CARRY[dsum, dcarry] + 1]

        digit != 0 && (last_nonzero = i)

        diff = T(digit) << offset
        ds |= diff

        mask <<= 3
        offset += 3
    end

    mask = typemax(T) << 3last_nonzero

    return GBTInt{T, U}(digits, ds | mask)
end

const INV = [0, 4, 5, 6, 1, 2, 3]
function -(gbt::GBTInt{T, U}) where {T, U}
    ds = typemax(T)
    N = maxdigits(T)

    offset = 0
    mask = T(7)
    for i in 1:N
        digit = gbt.digits & mask

        digit == mask && break

        digit >>= offset
        diff = T(@inbounds INV[digit + 1]) << offset
        ds ⊻= ~diff & mask

        mask <<= 3
        offset += 3
    end
    return GBTInt{T, U}(digits, ds)
end

-(lhs::GBTInt, rhs::GBTInt) = lhs + (-rhs)

function *(gbt::T, x::U) where {T <: GBTInt, U <: Unsigned}
    result = zero(T)
    iszero(x) && return result
    
    N = lb(x)
    mask = U(1)
    for i in 1:N
        x & mask == mask && (result += gbt)

        x >>= 1
        gbt += gbt
    end
    return result
end
*(gbt::GBTInt, x::Integer) = x < 0 ? -gbt * unsigned(-x) : gbt * unsigned(x)
*(x::Integer, gbt::GBTInt) = x < 0 ? -gbt * unsigned(-x) : gbt * unsigned(x)

<<(gbt::T, n::Integer) where T <: GBTInt = T(digits, gbt.digits << 3n)
function >>(gbt::GBTInt{T, U}, n::Integer) where {T, U}
    N = 3n
    ds = gbt.digits >> N

    invN = 8sizeof(T) - N
    fill = typemax(T) << invN

    return GBTInt{T, U}(digits, ds | fill)
end

function make_units()
    @assert sizeof(IntMax) <= 16 "GBT_UNITS overflow"
    units = Vector{Vec{3, Int}}(undef, MAX_DIGITS)
    @inbounds units[1] = Vec(1, 0, 0)
    for n in 2:MAX_DIGITS
        @inbounds units[n] = units[n - 1] - 2units[n - 1].yzx
    end
    # TODO: merge this into the recursion relation
    f(v) = v - v.zxy
    units .= f.(units)
    return units
end
const GBT_UNITS = make_units()

@inline function proj(n::Integer, i::Integer)
    @assert 1 <= n <= MAX_DIGITS
    @assert 0 <= i <= 6

    i == 0 && return Vec(0, 0, 0)

    p = Hexes.GBT_UNITS[n]
    if mod(i, 3) == 0
        p = p.zxy
    elseif mod(i, 3) == 2
        p = p.yzx
    end
    iseven(i) && (p = -p)

    return p
end

function proj(gbt::GBTInt{T}) where T
    p = Vec(0, 0, 0)

    iszero(gbt) && return p

    N = maxdigits(T)

    mask = T(7)
    digits = gbt.digits
    for i in 1:N
        digit = digits & mask
        digit == mask && break

        p += proj(i, digit)

        digits >>= 3
    end
    return p
end


norm(gbt::GBTInt) = sum(abs(proj(gbt))) >> 1

function rot(gbt::GBTInt{T, U}, i::Int) where {T, U}
    ds = typemax(T)
    N = maxdigits(T)

    mask = T(7)
    offset = 0
    for _ in 1:N
        digit = gbt.digits & mask

        digit == mask && break
        iszero(digit) && continue

        digit >>= offset

        digit = trunc(T, mod(digit + i - 1, 6) + 1)

        diff = digit << offset
        ds ⊻= ~diff & mask

        mask <<= 3
        offset += 3
    end
    return GBTInt{T, U}(digits, ds)
end
rotl60(gbt::GBTInt) = rot(gbt, 1)
rotr60(gbt::GBTInt) = rot(gbt, -1)
rotl120(gbt::GBTInt) = rot(gbt, 2)
rotr120(gbt::GBTInt) = rot(gbt, -2)
rot180(gbt::GBTInt) = -gbt

neighbour(gbt::GBTInt{T, U}, i::Int) where {T, U} = gbt + GBTInt{T, U}(Val{i}())

findgridsize(radius::Integer) = Int(SEVENS[nextpow7(1 + 3(radius + 1)radius)])
function findtype(::typeof(signed), N::Integer)
    U = decimaltype_signed(N)
    T = digitstype(U)
    return GBTInt{T, U}
end
function findtype(::typeof(unsigned), N::Integer)
    U = decimaltype_unsigned(N)
    T = digitstype(U)
    return GBTInt{T, U}
end

# 2D Mapping
cartesian(gbt::GBTInt; size = 1.0) = cartesian(Cube(gbt); size)
GBTInt{T, U}(xy::Vec{2}; size = 1.0) where {T, U} = GBTInt{T, U}(round(U, Cube(Val{:xy}(), xy, size)))
GBTInt{T}(xy::Vec{2}; size = 1.0) where T = GBTInt{T, decimaltype_signed(T)}(xy, size)
GBTInt{T}(::typeof(signed), xy::Vec{2}; size = 1.0) where T = GBTInt{T, decimaltype_signed(T)}(xy, size)
GBTInt{T}(::typeof(unsigned), xy::Vec{2}; size = 1.0) where T = GBTInt{T, decimaltype_unsigned(T)}(xy, size)
# TODO: find best digits type based on norm of xy
GBTInt(::Type{T}, xy::Vec{2}; size = 1.0) where T = GBTInt{UInt}(T, xy; size)


# Printing
const ZERO_ORD = UInt('0')
function show(io::IO, gbt::GBTInt{T, U}) where {T, U}
    N = maxdigits(T)
    vec = MVector{N, Char}('0' for _ in 1:N)

    mask = T(7)
    digits = gbt.digits
    for i in 1:N
        digit = digits & mask

        digit == mask && break

        vec[i] = Char(digit + ZERO_ORD)
        digits >>= 3
    end

    print(io, U <: Unsigned ? "ugbt" : "gbt", '"', reverse(vec)...,'"')
end