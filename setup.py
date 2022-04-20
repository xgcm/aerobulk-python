import os

from setuptools import find_packages  # , setup
from numpy.distutils.core import setup, Extension
from numpy.distutils.fcompiler import get_default_fcompiler, CompilerNotFound

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
        name="mod_aerobulk_wrap",  # this somehow needs to match the mod_aerobulk_wrap.* files. Ideally I would like to name this just aerobulk (for later.)
        sources=[
            "./src/aerobulk/src/mod_const.f90",
            "./src/aerobulk/src/mod_phymbl.f90",
            "./src/aerobulk/src/mod_skin_coare.f90",
            "./src/aerobulk/src/mod_skin_ecmwf.f90",
            "./src/aerobulk/src/mod_blk_andreas.f90",
            "./src/aerobulk/src/mod_common_coare.f90",
            "./src/aerobulk/src/mod_blk_coare3p0.f90",
            "./src/aerobulk/src/mod_blk_coare3p6.f90",
            "./src/aerobulk/src/mod_blk_ecmwf.f90",
            "./src/aerobulk/src/mod_blk_ncar.f90",
            "./src/aerobulk/src/mod_blk_neutral_10m.f90",
            "./src/aerobulk/src/mod_aerobulk_compute.f90",
            "./src/aerobulk/src/mod_aerobulk.f90",
            "./src/mod_aerobulk_wrap.f90",
            "./src/mod_aerobulk_wrap.pyf",
        ],
        extra_f90_compile_args=f90flags,
        # f2py_options=['--quiet'],
    )
]


## Lets populate the commented inputs after this is working
setup(
    name="aerobulk-python",
    description="General Circulation Model Postprocessing with xarray",
    url="https://github.com/xgcm/aerobulk-python",
    author="aerobulk-python Developers",
    author_email="julius@ldeo.columbia.edu",
    # license="GPLv3",
    # classifiers=[
    #     "Development Status :: 2 - Pre-Alpha",
    #     "Intended Audience :: Science/Research",
    #     "Topic :: Scientific/Engineering",
    #     "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
    #     "Operating System :: OS Independent",
    #     "Programming Language :: Python :: 3.7",
    #     "Programming Language :: Python :: 3.8",
    #     "Programming Language :: Python :: 3.9",
    # ],
    packages=find_packages(exclude=["docs", "tests", "tests.*", "docs.*"]),
    install_requires=install_requires,
    python_requires=">=3.7",
    # long_description=long_description,
    # long_description_content_type="text/x-rst",
    setup_requires="setuptools_scm",
    use_scm_version={
        # "write_to": "aerobulk-python/_version.py",
        "write_to": "src/_version.py",  # I would like to rename src to aerobulk similar to gsw at some point.
        "write_to_template": '__version__ = "{version}"',
        "tag_regex": r"^(?P<prefix>v)?(?P<version>[^\+]+)(?P<suffix>.*)?$",
    },
    ext_package="aerobulk",
    ext_modules=ext_modules,
)
