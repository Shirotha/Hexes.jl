module Hexes
    # TODO: include doc templates

    include("header.jl")

    include("util.jl")

    # cube coordinates
    include("cube/Cube.jl")

    # gbt
    include("gbt/GBT.jl")

    # conversion
    include("cube/gbt.jl")
    include("gbt/cube.jl")

    # shapes
    include("shapes/Neighbours.jl")
    include("shapes/Line.jl")
    include("shapes/Ring.jl")
    include("shapes/Circle.jl")
    include("shapes/Cone.jl")

end