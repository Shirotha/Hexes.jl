struct GBTIntDigits{T <: Unsigned, X} <: AbstractVector{T}
    digits::T
end

function maxdigits(ds::GBTIntDigits{T}) where T
    result = 0
    N = maxdigits(T)
    
    mask = T(7)
    digits = ds.digits
    for i in 1:N
        digit = digits & mask
        digit == mask && return result
        
        digits >>= 3
        result += 1
    end
    return result
end

size(ds::GBTIntDigits{T, Val{true}}) where T = (maxdigits(ds),)
size(::GBTIntDigits{T, Val{false}}) where T = (maxdigits(T),)

length(ds::GBTIntDigits{T, Val{true}}) where T = maxdigits(ds)
length(::GBTIntDigits{T, Val{false}}) where T = maxdigits(T)

eltype(::Type{GBTIntDigits{T}}) where T = T
eltype(::GBTIntDigits{T}) where T = T

function iterate(gbt::GBTIntDigits{T, Val{true}}) where T
    mask = T(7)
    value = gbt.digits & mask
    value == mask && return zero(T), typemax(Int)

    return value, 3
end
function iterate(ds::GBTIntDigits{T, Val{true}}, offset::Int) where T
    offset < 8sizeof(T) || return nothing

    digits = ds.digits >> offset

    mask = T(7)
    value = digits & mask
    value == mask && return nothing

    return value, offset + 3
end

function iterate(gbt::GBTIntDigits{T, Val{false}}) where T
    mask = T(7)
    value = gbt.digits & mask
    value == mask && return zero(T), 3

    return value, 3
end
function iterate(ds::GBTIntDigits{T, Val{false}}, offset::Int) where T
    offset < 8sizeof(T) || return nothing
    digits = ds.digits >> offset

    mask = T(7)
    value = digits & mask
    value == mask && return zero(T), offset + 3

    return value, offset + 3
end

function getindex(ds::GBTIntDigits{T}, d::Integer) where T
    @boundscheck 1 <= d <= maxdigits(T) || throw(BoundsError(ds, d))

    offset = 3(d - 1)
    digits = ds.digits >> offset

    mask = T(7)
    value = digits & mask
    value == mask && return zero(T)

    return value
end

digits(gbt::GBTInt{T}, early_exit::Bool = false) where T = GBTIntDigits{T, Val{early_exit}}(gbt.digits)
digits(gbt::GBTInt{T}, ::Val{X}) where {T, X} = GBTIntDigits{T, Val{X}}(gbt.digits)