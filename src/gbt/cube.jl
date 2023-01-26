function GBTInt{T, U}(cube::Cube{<:Integer}) where {T, U}
    x = GBTInt{T, U}(Val{1}())
    y = GBTInt{T, U}(Val{3}())
    z = GBTInt{T, U}(Val{5}())
    qrs = cube.qrs

    a = (qrs[1] - qrs[2]) >> 1
    b = (qrs[1] + qrs[2]) >> 1
    
    result = a * x + b * (y - z)
    isodd(qrs[3]) && (result -= z)

    return result
end
function GBTInt{T}(cube::Cube{<:Signed}) where T
    U = decimaltype_signed(T)
    return GBTInt{T, U}(cube)
end
function GBTInt{T}(cube::Cube{<:Unsigned}) where T
    U = decimaltype_unsigned(T)
    return GBTInt{T, U}(cube)
end
function GBTInt(cube::Cube{U}) where U <: Integer
    T = digitstype(U)
    return GBTInt{T, U}(cube)
end