[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/xgcm/aerobulk-python/main.svg)](https://results.pre-commit.ci/latest/github/xgcm/aerobulk-python/main)

# aerobulk-python
A python wrapper for aerobulk (https://github.com/brodeau/aerobulk)

## Installation

## aerobulk-python developer guide

These are the steps to install and develop the package locally

1. Set up a development environment.
```
mamba create -n aerobulk-python-dev python=3.9 dask numpy gfortran xarray pytest ipython
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
    python -m numpy.f2py --verbose -c --f90flags="-fdefault-real-8 -ffree-line-length-200 --std=gnu" ./source/fortran/aerobulk/src/mod_const.f90 ./source/fortran/aerobulk/src/mod_phymbl.f90 ./source/fortran/aerobulk/src/mod_skin_coare.f90 ./source/fortran/aerobulk/src/mod_skin_ecmwf.f90 ./source/fortran/aerobulk/src/mod_blk_andreas.f90 ./source/fortran/aerobulk/src/mod_common_coare.f90 ./source/fortran/aerobulk/src/mod_blk_coare3p0.f90 ./source/fortran/aerobulk/src/mod_blk_coare3p6.f90 ./source/fortran/aerobulk/src/mod_blk_ecmwf.f90 ./source/fortran/aerobulk/src/mod_blk_ncar.f90 ./source/fortran/aerobulk/src/mod_blk_neutral_10m.f90 ./source/fortran/aerobulk/src/mod_aerobulk_compute.f90 ./source/fortran/aerobulk/src/mod_aerobulk.f90   ./source/fortran/mod_aerobulk_wrap_skin.f90 ./source/fortran/mod_aerobulk_wrap_skin.pyf
    ```
    and

    ```
    python -m numpy.f2py --verbose -c --f90flags="-fdefault-real-8 -ffree-line-length-200 --std=gnu" ./source/fortran/aerobulk/src/mod_const.f90 ./source/fortran/aerobulk/src/mod_phymbl.f90 ./source/fortran/aerobulk/src/mod_skin_coare.f90 ./source/fortran/aerobulk/src/mod_skin_ecmwf.f90 ./source/fortran/aerobulk/src/mod_blk_andreas.f90 ./source/fortran/aerobulk/src/mod_common_coare.f90 ./source/fortran/aerobulk/src/mod_blk_coare3p0.f90 ./source/fortran/aerobulk/src/mod_blk_coare3p6.f90 ./source/fortran/aerobulk/src/mod_blk_ecmwf.f90 ./source/fortran/aerobulk/src/mod_blk_ncar.f90 ./source/fortran/aerobulk/src/mod_blk_neutral_10m.f90 ./source/fortran/aerobulk/src/mod_aerobulk_compute.f90 ./source/fortran/aerobulk/src/mod_aerobulk.f90   ./source/fortran/mod_aerobulk_wrap_noskin.f90 ./source/fortran/mod_aerobulk_wrap_noskin.pyf
    ```
4. Make sure things work
```
pytest tests/test_fortran.py
```
🎉

## Local Aerobulk compilation guide
Assumes that you have an environment with a gfortran compiler set up, have aerobulk cloned and that you are in the root aerobulk (not aerobulk-python) directory.

1. Build the aerobulk source

First copy the appropriate macro file to the source directory:
```
cp arch/make.macro_GnuLinux make.macro
```

those of us on mac needed to edit `make.macro` make this change
```diff
# These are needed for the C/C++ interface
- FF += -std=gnu -lstdc++
+ FF += -std=gnu
```

Then you should be able to run `make`

This will build:
- Object files `src/*.o`
- Mode files `mod/*.mod`
- Library file `lib/libaerobulk.a`
- Binary executables `bin/*x`

I was able to run one of the binaries with `bin/example_call_aerobulk.x`


<details><summary>Output</summary>
```
*********** COARE 3.0 *****************

 ===================================================================
                    ----- AeroBulk_init -----

     *** Bulk parameterization to be used => "coare3p0"
        ==> will use the Cool-skin & Warm-ayer scheme of `coare3p0` !
     *** Computational domain shape: Ni x Nj = 00002 x 00001
     *** Number of time records that will be treated:           1
     *** Number of iterations in bulk algos: nb_iter  =   10
     *** Filling the `mask` array...
         ==> no points need to be masked! :)
     *** Type of prescribed air humidity  `specific humidity [kg/kg]`
 ===================================================================
 ===================================================================
                    ----- AeroBulk_bye -----
 ===================================================================


 ---------------------------------------------------------------------
     Parameter           | Unstable ASL |  Stable ASL  | units
 ---------------------------------------------------------------------
  Wind speed at zu       =   5.00000000       5.00000000      m/s
     SST                 =   22.0000000       22.0000000      deg.C
  Abs. temperature at zt =   20.0000000       25.0000000      deg.C
  Pot. temperature at zt =   20.0134144       25.0150242      deg.C

  Sensible heat flux: QH =  -15.1545086       17.8423691      W/m**2
   Latent  heat flux: QL =  -81.3846741      -50.8408585      W/m**2
   Evaporation:     Evap =  -2.87061906      -1.79333246      mm/day
  Skin temperature: SSST =   21.7219677       21.7576351      deg.C
  Tau_x                  =   3.57834995E-02   1.73626728E-02  N/m**2
  Tau_y                  =   0.00000000       0.00000000      N/m**2
  Tau                    =   3.57834995E-02   1.73626728E-02  N/m**2



  *********** COARE 3.6 *****************

 ===================================================================
                    ----- AeroBulk_init -----

     *** Bulk parameterization to be used => "coare3p6"
        ==> will use the Cool-skin & Warm-ayer scheme of `coare3p6` !
     *** Computational domain shape: Ni x Nj = 00002 x 00001
     *** Number of time records that will be treated:           1
     *** Number of iterations in bulk algos: nb_iter  =   10
     *** Filling the `mask` array...
         ==> no points need to be masked! :)
     *** Type of prescribed air humidity  `specific humidity [kg/kg]`
 ===================================================================
 ===================================================================
                    ----- AeroBulk_bye -----
 ===================================================================


 ---------------------------------------------------------------------
     Parameter           | Unstable ASL |  Stable ASL  | units
 ---------------------------------------------------------------------
  Wind speed at zu       =   5.00000000       5.00000000      m/s
     SST                 =   22.0000000       22.0000000      deg.C
  Abs. temperature at zt =   20.0000000       25.0000000      deg.C
  Pot. temperature at zt =   20.0134144       25.0150242      deg.C

  Sensible heat flux: QH =  -15.3865499       17.0818920      W/m**2
   Latent  heat flux: QL =  -83.0788422      -48.4459152      W/m**2
   Evaporation:     Evap =  -2.93033028      -1.70883954      mm/day
  Skin temperature: SSST =   21.7057972       21.7485600      deg.C
  Tau_x                  =   3.21817845E-02   1.51577257E-02  N/m**2
  Tau_y                  =   0.00000000       0.00000000      N/m**2
  Tau                    =   3.21817845E-02   1.51577257E-02  N/m**2



  *********** ECMWF *****************

 ===================================================================
                    ----- AeroBulk_init -----

     *** Bulk parameterization to be used => "ecmwf"
        ==> will use the Cool-skin & Warm-ayer scheme of `ecmwf` !
     *** Computational domain shape: Ni x Nj = 00002 x 00001
     *** Number of time records that will be treated:           1
     *** Number of iterations in bulk algos: nb_iter  =   10
     *** Filling the `mask` array...
         ==> no points need to be masked! :)
     *** Type of prescribed air humidity  `specific humidity [kg/kg]`
 ===================================================================
 ===================================================================
                    ----- AeroBulk_bye -----
 ===================================================================


 ---------------------------------------------------------------------
     Parameter           | Unstable ASL |  Stable ASL  | units
 ---------------------------------------------------------------------
  Wind speed at zu       =   5.00000000       5.00000000      m/s
     SST                 =   22.0000000       22.0000000      deg.C
  Abs. temperature at zt =   20.0000000       25.0000000      deg.C
  Pot. temperature at zt =   20.0134144       25.0150242      deg.C

  Sensible heat flux: QH =  -14.3822346       17.6531811      W/m**2
   Latent  heat flux: QL =  -80.2958984      -52.4623947      W/m**2
   Evaporation:     Evap =  -2.83224440      -1.85053933      mm/day
  Skin temperature: SSST =   21.7325401       21.7630310      deg.C
  Tau_x                  =   3.84389125E-02   1.93256922E-02  N/m**2
  Tau_y                  =   0.00000000       0.00000000      N/m**2
  Tau                    =   3.84389125E-02   1.93256922E-02  N/m**2



  *********** NCAR *****************

 ===================================================================
                    ----- AeroBulk_init -----

     *** Bulk parameterization to be used => "ncar"
     *** Cool-skin & Warm-layer schemes will NOT be used!
     *** Computational domain shape: Ni x Nj = 00002 x 00001
     *** Number of time records that will be treated:           1
     *** Number of iterations in bulk algos: nb_iter  =   10
     *** Filling the `mask` array...
         ==> no points need to be masked! :)
     *** Type of prescribed air humidity  `specific humidity [kg/kg]`
 ===================================================================
 ===================================================================
                    ----- AeroBulk_bye -----
 ===================================================================


 ---------------------------------------------------------------------
     Parameter           | Unstable ASL |  Stable ASL  | units
 ---------------------------------------------------------------------
  Wind speed at zu       =   5.00000000       5.00000000      m/s
     SST                 =   22.0000000       22.0000000      deg.C
  Abs. temperature at zt =   20.0000000       25.0000000      deg.C
  Pot. temperature at zt =   20.0134144       25.0150242      deg.C

  Sensible heat flux: QH =  -16.6969528       10.7261686      W/m**2
   Latent  heat flux: QL =  -88.4781876      -71.9012222      W/m**2
   Evaporation:     Evap =  -3.12166286      -2.53679895      mm/day
  Tau_x                  =   3.58519591E-02   2.77329944E-02  N/m**2
  Tau_y                  =   0.00000000       0.00000000      N/m**2
  Tau                    =   3.58519591E-02   2.77329944E-02  N/m**2



  *********** ANDREAS *****************

 ===================================================================
                    ----- AeroBulk_init -----

     *** Bulk parameterization to be used => "andreas"
     *** Cool-skin & Warm-layer schemes will NOT be used!
     *** Computational domain shape: Ni x Nj = 00002 x 00001
     *** Number of time records that will be treated:           1
     *** Number of iterations in bulk algos: nb_iter  =   10
     *** Filling the `mask` array...
         ==> no points need to be masked! :)
     *** Type of prescribed air humidity  `specific humidity [kg/kg]`
 ===================================================================
 ===================================================================
                    ----- AeroBulk_bye -----
 ===================================================================


 ---------------------------------------------------------------------
     Parameter           | Unstable ASL |  Stable ASL  | units
 ---------------------------------------------------------------------
  Wind speed at zu       =   5.00000000       5.00000000      m/s
     SST                 =   22.0000000       22.0000000      deg.C
  Abs. temperature at zt =   20.0000000       25.0000000      deg.C
  Pot. temperature at zt =   20.0134144       25.0150242      deg.C

  Sensible heat flux: QH =  -14.4129944       15.1970539      W/m**2
   Latent  heat flux: QL =  -74.4637756      -51.7018318      W/m**2
   Evaporation:     Evap =  -2.62721038      -1.82412970      mm/day
  Tau_x                  =   3.02770734E-02   1.79436151E-02  N/m**2
  Tau_y                  =   0.00000000       0.00000000      N/m**2
  Tau                    =   3.02770734E-02   1.79436151E-02  N/m**2
```
</details>
