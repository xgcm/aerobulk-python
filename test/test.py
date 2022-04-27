from time import time
import numpy as np
from aerobulk import flux_noskin


def test_smoke_test():
    # I am very confused about shape.
    # We want Nt to be the *slowest varying dimension*
    # For C-order arrays, would give (Nt, Ny, Nx)
    # For F-order arrays, would give (Nx, Ny, Nt)
    # I expected that the shape passed from Python would reverse when it hits Fortran,
    # but this did not happen.
    shape = (200, 200, 10)
    order = "F"
    jt = 1
    nt = 1
    sst = np.full(shape, 290.0, order=order)
    t_zt = np.full(shape, 290.0, order=order)
    hum_zt = np.full(shape, 0.001, order=order)
    u_zu = np.full(shape, 1.0, order=order)
    v_zu = np.full(shape, -1.0, order=order)
    slp = np.full(shape, 101000.0, order=order)
    tic = time()
    ql, qh, tau_x, tau_y, evap = flux_noskin(sst, t_zt, hum_zt, u_zu, v_zu, slp)
    toc = time()
    print(f"ORDER: {order} | TIME: {toc-tic}")


def test_aerobulk_toy_results():
    # This test aims to check the python API against the results posted in the README of
    # aerobulk (https://github.com/brodeau/aerobulk#-giving-aerobulk-a-first-try-in-interactive-toy-mode)
    niter = 8
    zu = 10
    zt = 2
    sst = 22 + 273  # 22 C
    t_zt = 20 + 273  # 20 C
    hum_zt = 12  # g/kg
    u_zu = 5
    v_zu = 0

    # start with only one algo
    algo = "coare3p0"
    expected_tau_x = 36.198
    expected_evap = 3.1059
    expected_ql = -88.031
    expected_qh = -17.833

    ql, qh, tau_x, tau_y, evap = flux_noskin(
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        zu=zu,
        zt=zt,
        niter=niter,
    )
    # convert to the units displayed...geez this is convoluted...
    evap = 3600 * 24 * evap
    tau_x = 1e3 * tau_x

    print(ql, qh, tau_x, evap)
    assert 1 == 0
