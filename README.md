[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/xgcm/aerobulk-python/main.svg)](https://results.pre-commit.ci/latest/github/xgcm/aerobulk-python/main)

# aerobulk-python
A python wrapper for aerobulk (https://github.com/brodeau/aerobulk)

## Installation

## aerobulk-python developer guide

These are the steps to install and develop the package locally

1. Set up a development environment.
```
mamba create -n aerobulk-python-dev python=3.9 numpy gfortran xarray pytest ipython
conda activate aerobulk-python-dev
```

2. Clone the repository

Make sure to clone the repo with `--recursive` to also pull the git submodule!

```
git clone --recursive git@github.com:xgcm/aerobulk-python.git
```

3. Compile/Install aerobulk-python (this has to be rerun after every change to the fortran code/wrapper/signature file)
    Run these commands from within the `aerobulk-python` folder.

    a. Use pip to install aerobulk locally (this will install the python package and compile). 
    ```
    pip install -e .
    ```
    b. Manual version (you will have to install the python module separately)
    ```
    python -m numpy.f2py --verbose -c --f90flags="-fdefault-real-8 -ffree-line-length-200 --std=gnu" ./source/fortran/aerobulk/src/mod_const.f90 ./source/fortran/aerobulk/src/mod_phymbl.f90 ./source/fortran/aerobulk/src/mod_skin_coare.f90 ./source/fortran/aerobulk/src/mod_skin_ecmwf.f90 ./source/fortran/aerobulk/src/mod_blk_andreas.f90 ./source/fortran/aerobulk/src/mod_common_coare.f90 ./source/fortran/aerobulk/src/mod_blk_coare3p0.f90 ./source/fortran/aerobulk/src/mod_blk_coare3p6.f90 ./source/fortran/aerobulk/src/mod_blk_ecmwf.f90 ./source/fortran/aerobulk/src/mod_blk_ncar.f90 ./source/fortran/aerobulk/src/mod_blk_neutral_10m.f90 ./source/fortran/aerobulk/src/mod_aerobulk_compute.f90 ./source/fortran/aerobulk/src/mod_aerobulk.f90 ./source/fortran/mod_aerobulk_wrap.f90 ./source/fortran/mod_aerobulk_wrap.pyf
    ```
    
4. Make sure things work
```
pytest test/test.py
```    
🎉
