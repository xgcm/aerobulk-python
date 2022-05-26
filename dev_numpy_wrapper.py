import aerobulk.aerobulk.mod_aerobulk_wrap as aero
import numpy as np


def test_shape(shape):

    order = "F"

    sst = np.full(shape, 290.0, order=order)
    t_zt = np.full(shape, 280.0, order=order)
    hum_zt = np.full(shape, 0.001, order=order)
    u_zu = np.full(shape, 1.0, order=order)
    v_zu = np.full(shape, -1.0, order=order)
    slp = np.full(shape, 101000.0, order=order)

    algo = "coare3p0"
    zt = (10,)
    zu = (2,)
    niter = 4

    ql, qh, tau_x, tau_y, evap = aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        algo,
        zt,
        zu,
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        slp,
        niter,
    )
    print(f"original:{shape} | output:{ql.shape}")
    # print("ql", ql.shape)
    # print("qh", qh.shape)
    # print("tau_x", tau_x.shape)
    # print("tau_y", tau_y.shape)
    # print("evap", evap.shape)

    # for sh in [
    # (200, 200, 10),  # original:(200, 200, 10) | output:(200, 200, 10)
    # (200, 200, 1),  # original:(200, 200, 1) | output:(200, 200, 1)
    # (200, 200),  # original:(200, 200) | output:(200, 200, 1)
    # (200),  # original:200 | output:(200, 1, 1)
    # (),  # original:() | output:(1, 1, 1)
    # (200, 200, 10, 4),  # ValueError: too many axes: 4 (effrank=4), expected rank=3
    # (200, 200, 0),  # ValueError: unexpected array size: new_size=40000, got array with arr_size=0
    # (200, 0, 3), #ValueError: unexpected array size: new_size=40000, got array with arr_size=0
    (1, 1, 1),  # original:(1, 1, 1) | output:(1, 1, 1)


# ]:
#    test_shape(sh)
