@testset verbose=true "Cube" begin
    @testset "Properties" begin
        @test Cube(1, 2, -3).qrs === Vec(1, 2, -3)
        @test Cube(1.0, 0, -1.0).qq === Vec(1.0, 1.0)
    end
    @testset "Constructors" begin
        @test eltype(Cube(1, 2.0)) === Float64
        let cube = Cube(1, 2)
            @test Cube(cube) === cube
        end
        @test Cube(Val{:rs}(), 1, 2).qrs === Vec(-3, 1, 2)
        @test Cube{Int}(Val(1)).qrs === Vec(1, -1, 0)
        @test zero(Cube{Int}).qrs === Vec(0, 0, 0)
        @test Cube{Int}(Val{:xy}(), Vec(0, sqrt(3))).qrs === Vec(0, 1, -1)
        @test @Cube(s = 1, q = -1).qrs === Vec(-1, 0, 1)
    end
    @testset "Arithmetics" begin
        let a = Cube{Float64}(1, 2, -3), b = Cube{Int}(-2, 0, 2)
            @test (a + b).qrs === a.qrs + b.qrs
            @test (a - b).qrs === a.qrs - b.qrs
            @test (3a).qrs === 3a.qrs
            @test (a / 2).qrs === a.qrs / 2
            @test norm(a) == 3.0
            @test norm(b) == 2
            @test dot(a, b) == -8.0
        end
    end
    @testset "Rotation" begin
        let x = Cube{Int}(Val{1}())
            @test rotr120(x) === Cube{Int}(Val{5}())
            @test rotl120(x) === Cube{Int}(Val{3}())
            @test rot180(x) === Cube{Int}(Val{4}())
            @test rotr60(x) === Cube{Int}(Val{6}())
            @test rotl60(x) === Cube{Int}(Val{2}())
        end
    end
    @testset "Rounding" begin
        let x = Cube(0.6, 1.3, -1.9)
            @test floor(x) === Cube(1.0, 1.0, -2.0)
            @test ceil(x) === Cube(1.0, 2.0, -3.0)
            @test round(Int, x) === Cube(1, 1, -2)
        end
    end
    @testset "Iteration" begin
        @test neighbour(Cube(3, 2), 1) === Cube(3, 2) + Cube{Int}(Val{1}())
        let x = Cube(1, 2), ns = collect(neighbours(x))
            @test length(ns) == 6
            @test all(in(ns), (neighbour(x, i) for i in 1:6))
        end
        let a = Cube(-1, -3), b = Cube(3, 3), l = line(a, b)
            @test length(l) == norm(a - b)
            @test all(==(1), (norm(l[i] - l[i - 1]) for i in eachindex(l) if i != firstindex(l)))
        end
        let x = Cube(1, 1), ys = ring(x, 3)
            @test length(ys) == 18
            @test all(y -> norm(x - y) == 3, ys)
        end
        let x = Cube(3, -1), ys = circle(x, 2)
            @test length(ys) == 19
            @test all(y -> norm(x - y) <= 2, ys)
        end
        let x = Cube(1, 2), ys = cone(x, 4, 1)
            @test length(ys) == 15
            @test all(y -> norm(x - y) <= 4, ys)
        end
    end
end