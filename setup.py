import os
import numpy.distutils.command.sdist  # noqa
import setuptools  # noqa
from numpy.distutils.core import Extension, setup
from numpy.distutils.fcompiler import get_default_fcompiler

# figure out which compiler we're going to use
compiler = get_default_fcompiler()
# set some fortran compiler-dependent flags
f90flags = []
if compiler == "gnu95":
    f90flags.append("-fdefault-real-8")
    f90flags.append("-ffree-line-length-200")
elif compiler == "intel" or compiler == "intelem":
    f90flags.append("-132")
#  Set aggressive optimization level
f90flags.append("-O3")
#  Suppress all compiler warnings (avoid huge CI log files)
f90flags.append("-w")

# Extension modules
ext_modules = [
    Extension(
        name="mod_aerobulk_wrap_noskin",
        sources=[
            "source/fortran/aerobulk/src/mod_const.f90",
            "source/fortran/aerobulk/src/mod_phymbl.f90",
            "source/fortran/aerobulk/src/mod_skin_coare.f90",
            "source/fortran/aerobulk/src/mod_skin_ecmwf.f90",
            "source/fortran/aerobulk/src/mod_blk_andreas.f90",
            "source/fortran/aerobulk/src/mod_common_coare.f90",
            "source/fortran/aerobulk/src/mod_blk_coare3p0.f90",
            "source/fortran/aerobulk/src/mod_blk_coare3p6.f90",
            "source/fortran/aerobulk/src/mod_blk_ecmwf.f90",
            "source/fortran/aerobulk/src/mod_blk_ncar.f90",
            "source/fortran/aerobulk/src/mod_blk_neutral_10m.f90",
            "source/fortran/aerobulk/src/mod_aerobulk_compute.f90",
            "source/fortran/aerobulk/src/mod_aerobulk.f90",
            "source/fortran/mod_aerobulk_wrap_noskin.f90",
            "source/fortran/mod_aerobulk_wrap_noskin.pyf",
        ],
        extra_f90_compile_args=f90flags,
    ),
    Extension(
        name="mod_aerobulk_wrap_skin",
        sources=[
            "source/fortran/aerobulk/src/mod_const.f90",
            "source/fortran/aerobulk/src/mod_phymbl.f90",
            "source/fortran/aerobulk/src/mod_skin_coare.f90",
            "source/fortran/aerobulk/src/mod_skin_ecmwf.f90",
            "source/fortran/aerobulk/src/mod_blk_andreas.f90",
            "source/fortran/aerobulk/src/mod_common_coare.f90",
            "source/fortran/aerobulk/src/mod_blk_coare3p0.f90",
            "source/fortran/aerobulk/src/mod_blk_coare3p6.f90",
            "source/fortran/aerobulk/src/mod_blk_ecmwf.f90",
            "source/fortran/aerobulk/src/mod_blk_ncar.f90",
            "source/fortran/aerobulk/src/mod_blk_neutral_10m.f90",
            "source/fortran/aerobulk/src/mod_aerobulk_compute.f90",
            "source/fortran/aerobulk/src/mod_aerobulk.f90",
            "source/fortran/mod_aerobulk_wrap_skin.f90",
            "source/fortran/mod_aerobulk_wrap_skin.pyf",
        ],
        extra_f90_compile_args=f90flags,
        f2py_options=["--quiet"],
    ),
]

# Minimal setup call - most configuration is now in pyproject.toml
setup(
    ext_package="aerobulk",
    ext_modules=ext_modules,
)
