@testset verbose=true "GBTInt" begin
    @testset "Constructors" begin
        @test GBTInt(42).digits === ~UInt128(0b001_111)
        @test GBTInt{UInt8}(42).digits === ~UInt8(0b001_111)
        @test gbt"60".digits === ~UInt8(0b001_111)
        @test GBTInt((0, 6, 1)).digits === ~UInt16(0b110_001_111)
        @test GBTInt((0,)).digits === typemax(UInt8)
    end
    @testset "Digits" begin
        let x = GBTInt{UInt16}(43)
            let ds = digits(x, false)
                @test length(ds) == 5
                @test collect(ds) == [1, 6, 0, 0, 0]
                @test ds[2] == 6
            end
            let ds = digits(x, Val{true}())
                @test length(ds) == 2
                @test collect(ds) == [1, 6]
            end
        end
    end
    @testset "Arithmetics" begin
        let a = gbt"25", b = gbt"62", c = gbt"10"
            @test a + b == c
            @test c - a == b
            @test b - c == -a
            @test iszero(a - a)
        end

        function test_digit_add(i::Int, j::Int)
            a = GBTInt((i,))
            b = GBTInt((j,))
            result = GBTInt((Hexes.SUM[i + 1, j + 1], Hexes.CARRY[i + 1, j + 1]))

            return a + b == result
        end
        @test all(test_digit_add(i, j) for i in 0:6, j in 0:6)

        let a = gbt"1", b = gbt"62"
            @test 3a == b
        end
        let a = gbt"3", b = gbt"30"
            @test a << 1 == b
            @test b >> 1 == a 
        end
        
        function test_projection(q::Int, r::Int)
            s = -(q + r)
            x = SGBTInt((1,))
            y = SGBTInt((3,))
            z = SGBTInt((5,))
            result = ((q - r) >> 1) * x + ((q + r) >> 1) * (y - z)
            isodd(s) && (result -= z)
            return proj(result) === Vec(q, r, s)
        end
        @test all(test_projection(q, r) for q in -5:5, r in -5:5)

        let x = gbt"64", xr60 = gbt"53", xl60 = gbt"15", xr120 = gbt"42", xl120 = gbt"26", x180 = gbt"31"
            @test rotl60(x) == xl60
            @test rotr60(x) == xr60
            @test rotl120(x) == xl120
            @test rotr120(x) == xr120
            @test rot180(x) == x180
        end
    end
end