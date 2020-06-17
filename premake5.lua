dst = "_build"
gen = "gen"

include "tools/gen_ir"
include "tools/gen_backends"

workspace "libfirm"

    configurations { "Debug", "Release" }
    platforms { "Win64" }
    architecture "x64"
    startproject "libfirm"

    location(dst)

    filter "action:vs2019"
        characterset "MBCS"

    filter {}

    include "libfirm"
