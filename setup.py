import os

# We need to import setuptools here in order for it to persist in sys.modules.
# Its presence/absence is used in subclassing setup in numpy/distutils/core.py.
# However, we need to run the distutils version of sdist, so import that first
# so that it is in sys.modules
import numpy.distutils.command.sdist  # noqa
import setuptools  # noqa
from numpy.distutils.core import Extension, setup
from numpy.distutils.fcompiler import get_default_fcompiler

# Trying this from the numpy setup.py


here = os.path.dirname(__file__)
with open(os.path.join(here, "README.md"), encoding="utf-8") as f:
    long_description = f.read()

install_requires = [
    "numpy",
]

# figure out which compiler we're going to use
compiler = get_default_fcompiler()
# set some fortran compiler-dependent flags
f90flags = []
if compiler == "gnu95":
    # f90flags.append('-fno-range-check')
    # f90flags.append('-ffree-form')
    # These are the flags I used in the manual version (might have some more here)
    f90flags.append("-fdefault-real-8")
    f90flags.append("-ffree-line-length-200")
elif compiler == "intel" or compiler == "intelem":
    f90flags.append("-132")
#  Set aggressive optimization level
f90flags.append("-O3")
#  Suppress all compiler warnings (avoid huge CI log files)
f90flags.append("-w")

# for this API we will only expose a single extension?
ext_modules = [
    Extension(
        name="mod_aerobulk_wrap",
        sources=[
            "./source/fortran/aerobulk/src/mod_const.f90",
            "./source/fortran/aerobulk/src/mod_phymbl.f90",
            "./source/fortran/aerobulk/src/mod_skin_coare.f90",
            "./source/fortran/aerobulk/src/mod_skin_ecmwf.f90",
            "./source/fortran/aerobulk/src/mod_blk_andreas.f90",
            "./source/fortran/aerobulk/src/mod_common_coare.f90",
            "./source/fortran/aerobulk/src/mod_blk_coare3p0.f90",
            "./source/fortran/aerobulk/src/mod_blk_coare3p6.f90",
            "./source/fortran/aerobulk/src/mod_blk_ecmwf.f90",
            "./source/fortran/aerobulk/src/mod_blk_ncar.f90",
            "./source/fortran/aerobulk/src/mod_blk_neutral_10m.f90",
            "./source/fortran/aerobulk/src/mod_aerobulk_compute.f90",
            "./source/fortran/aerobulk/src/mod_aerobulk.f90",
            "./source/fortran/mod_aerobulk_wrap.f90",
            "./source/fortran/mod_aerobulk_wrap.pyf",
        ],
        extra_f90_compile_args=f90flags,
        # f2py_options=['--quiet'],
    )
]

setup(
    name="aerobulk-python",
    description="General Circulation Model Postprocessing with xarray",
    url="https://github.com/xgcm/aerobulk-python",
    author="aerobulk-python Developers",
    author_email="julius@ldeo.columbia.edu",
    license="GPLv3",
    classifiers=[
        "Development Status :: 2 - Pre-Alpha",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
    ],
    install_requires=install_requires,
    python_requires=">=3.8",
    # long_description=long_description,
    # long_description_content_type="text/x-rst",
    setup_requires="setuptools_scm",
    use_scm_version={
        "write_to": "source/aerobulk/_version.py",
        "write_to_template": '__version__ = "{version}"',
        "tag_regex": r"^(?P<prefix>v)?(?P<version>[^\+]+)(?P<suffix>.*)?$",
    },
    package_dir={"": "source"},
    packages=["aerobulk"],
    ext_package="aerobulk.aerobulk",
    ext_modules=ext_modules,
)
