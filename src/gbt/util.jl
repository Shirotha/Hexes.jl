# integer log2
lb(x::T) where T <: Unsigned = 8sizeof(T) - leading_zeros(x)
lb(x) = lb(unsigned(x))

# integer types from required bits
nextbitssize(bits::Integer) = max(nextpow2(bits), 8)
nextunsignedtype(bits::Integer) = eval(Symbol(:UInt, nextbitssize(bits)))
nextsignedtype(bits::Integer) = eval(Symbol(:Int, nextbitssize(bits + 1)))

# digits fitting in size bytes
maxdigits(size::Int) = (size << 3) ÷ 3
maxdigits(::Type{T}) where T <: Unsigned = maxdigits(sizeof(T))

# smallest type that can hold a value converted from digits-type T
decimaltype_unsigned(::Type{T}) where T <: Unsigned = nextunsignedtype(3maxdigits(T))
decimaltype_signed(::Type{T}) where T <: Unsigned = nextsignedtype(3maxdigits(T))

decimaltype_unsigned(max::Integer) = nextunsignedtype(3maxdigits(nextpow2(lb(max) >> 3)))
decimaltype_signed(max::Integer) = nextsignedtype(3maxdigits(nextpow2((lb(max) >> 3) + 1)))

# biggest integer types
const UIntMax = nestwhile(T -> T <: Unsigned, widen, UInt8)
const IntMax = eval(Symbol(:Int, 8sizeof(UIntMax)))
# maximum digits supported
const MAX_DIGITS = maxdigits(UIntMax)
# power of sevens LUT
const SEVENS = nestlist(x -> 7x, MAX_DIGITS, one(UIntMax))
@assert all(i -> SEVENS[i] == 7SEVENS[i - 1], 2:MAX_DIGITS) "overflow"

# next largest power of 7
nextpow7(x) = findfirst(>(x), SEVENS)

# needed digits to represent all numbers upto max
neededdigits(max) = Int(ceil(log(7, max)))
neededdigits(::Type{T}) where T <: Integer = neededdigits(typemax(T))

# digits-type needed to represent all values from T
digitstype(::Type{T}) where T <: Integer = nextunsignedtype(3neededdigits(T))
digitstype(::T) where T <: Integer = digitstype(T)