from time import time
import numpy as np
import aerobulk.mod_aerobulk_wrap as aero


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
    calgo = "andreas"
    zt = 10.0
    zu = 10.0
    sst = np.full(shape, 290.0, order=order)
    t_zt = np.full(shape, 290.0, order=order)
    hum_zt = np.full(shape, 0.001, order=order)
    u_zu = np.full(shape, 1.0, order=order)
    v_zu = np.full(shape, -1.0, order=order)
    slp = np.full(shape, 101000.0, order=order)
    niter = 15  # make optional
    tic = time()
    ql, qh, tau_x, tau_y, evap = aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        calgo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, niter
    )
    toc = time()
    print(f"ORDER: {order} | TIME: {toc-tic}")
