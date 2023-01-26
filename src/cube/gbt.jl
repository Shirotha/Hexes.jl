Cube{T}(index::Val) where T = Cube{T}(GBT(index))
Cube{T}(gbt::GBT) where T = Cube{T}(proj(gbt))
Cube(gbt::GBTInt{T, U}) where {T, U} = Cube{U}(proj(gbt))