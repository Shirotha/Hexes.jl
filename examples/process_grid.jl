include("header.jl")

const R = 10
const N = findgridsize(R)
const T = findtype(signed, N)

dN = N - 1 - 3(R + 1)R

println("circle with radius $R needs a length $N array to store (wasting $dN elements), storing it using a $(eltype(T)).")

grid = Vector{Int}(undef, N)

for i in 1:N
    gbt = T(i - 1)
    grid[i] = norm(gbt)
end

for gbt in ring(gbt"42", 5)
    a = Cube(gbt)
    b = rotl120(a)
    cross = a.rsq * b.sqr - a.sqr * b.rsq
    grid[gbt[] + 1] -= sum(cross * cross)
end

for cube in line(@Cube(q = -2, s = 3), @Cube(s = -3, r = 3))
    gbt = T(cube)
    println("at $gbt: $(grid[gbt[] + 1])")
end