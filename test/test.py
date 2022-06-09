import numpy as np
import pytest
from aerobulk import flux_noskin, flux_skin
from aerobulk.flux import VALID_ALGOS, VALID_ALGOS_SKIN


@pytest.mark.parametrize(
    "algo",
    VALID_ALGOS
    + [pytest.param("wrong", marks=pytest.mark.xfail(strict=True, raises=ValueError))],
)
def test_smoke_noskin(algo):
    # I am very confused about shape.
    # We want Nt to be the *slowest varying dimension*
    # For C-order arrays, would give (Nt, Ny, Nx)
    # For F-order arrays, would give (Nx, Ny, Nt)
    # I expected that the shape passed from Python would reverse when it hits Fortran,
    # but this did not happen.
    shape = (200, 200, 10)
    order = "F"
    sst = np.full(shape, 290.0, order=order)
    t_zt = np.full(shape, 280.0, order=order)
    hum_zt = np.full(shape, 0.001, order=order)
    u_zu = np.full(shape, 1.0, order=order)
    v_zu = np.full(shape, -1.0, order=order)
    slp = np.full(shape, 101000.0, order=order)
    flux_noskin(sst, t_zt, hum_zt, u_zu, v_zu, slp, algo=algo)


@pytest.mark.parametrize(
    "algo",
    VALID_ALGOS_SKIN
    + [
        pytest.param("wrong", marks=pytest.mark.xfail(strict=True, raises=ValueError)),
        pytest.param("ncar", marks=pytest.mark.xfail(strict=True, raises=ValueError)),
    ],
)
def test_smoke_skin(algo):
    # I am very confused about shape.
    # We want Nt to be the *slowest varying dimension*
    # For C-order arrays, would give (Nt, Ny, Nx)
    # For F-order arrays, would give (Nx, Ny, Nt)
    # I expected that the shape passed from Python would reverse when it hits Fortran,
    # but this did not happen.
    shape = (200, 200, 10)
    order = "F"
    sst = np.full(shape, 290.0, order=order)
    t_zt = np.full(shape, 280.0, order=order)
    hum_zt = np.full(shape, 0.001, order=order)
    u_zu = np.full(shape, 1.0, order=order)
    v_zu = np.full(shape, -1.0, order=order)
    rad_sw = np.full(shape, 0.0, order=order)
    rad_lw = np.full(shape, 350.0, order=order)
    slp = np.full(shape, 101000.0, order=order)
    flux_skin(sst, t_zt, hum_zt, u_zu, v_zu, rad_sw, rad_lw, slp, algo=algo)
