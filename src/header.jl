using SIMD
using LinearAlgebra


import Base: getproperty, setproperty!, propertynames
import Base: Pairs

import Base: convert, promote_rule
import Base: ==, iszero, <, isless
import Base: digits

import Base: getindex, setindex, @propagate_inbounds

import Base: +, -, *, /, adjoint, inv, zero, <<, >>
import Base: floor, ceil, round, rem, widen
import LinearAlgebra: norm, dot, cross
import LinearAlgebra: rot180

import Base: position

import Base: iterate, length, eltype
import Base: size, getindex, setindex!, IndexStyle, similar, axes
import Base: keys, firstindex, lastindex, OneTo

import Base: _nextpow2 as nextpow2

import SIMD: Vec

import Base: show


abstract type Hex{T} <: Number end

export Hex, Cube, GBT, GBTInt, GBTFloat
export @Cube
export SGBTInt8, SGBTInt16, SGBTInt32, SGBTInt64, SGBTInt128, SGBTInt
export UGBTInt8, UGBTInt16, UGBTInt32, UGBTInt64, UGBTInt128, UGBTInt
export @gbt_str, @sgbt_str, @ugbt_str

export norm, dot
export rotr60, rotl60, rotr120, rotl120, rot180, rot
export neighbour
export cartesian

export digits
export proj

export findgridsize, findtype

export neighbours, line, ring, circle, cone