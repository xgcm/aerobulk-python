import aerobulk.aerobulk.mod_aerobulk_wrap as aero
import numpy as np
import xarray as xr
from aerobulk import flux_noskin

# flux_noskin(sst, t_zt, hum_zt, u_zu, v_zu, slp)


# shape = (200, 200, 10)


def test_shape(shape):
    print("LESSSSS GOOOO")
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


def flux_xr(
    sst, t_zt, hum_zt, u_zu, v_zu, slp=101000.0, algo="coare3p0", zt=10, zu=2, niter=1
):
    # TODO do we need to make the "time" dimension special?

    sst, t_zt, hum_zt, u_zu, v_zu, slp = xr.broadcast(
        sst, t_zt, hum_zt, u_zu, v_zu, slp
    )

    if len(sst.dims) < 3:
        # TODO promote using expand_dims?
        raise NotImplementedError
    if len(sst.dims) > 4:
        # TODO iterate over extra dims? Or reshape?
        raise NotImplementedError

    out_vars = xr.apply_ufunc(
        flux_noskin,
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        slp,
        input_core_dims=[()] * 6,
        output_core_dims=[()] * 5,
        # TODO dask="parallelized"
        kwargs=dict(
            algo=algo,
            zt=zt,
            zu=zu,
            niter=niter,
        ),
    )

    if not isinstance(out_vars, tuple) or len(out_vars) != 5:
        raise TypeError("F2Py returned unexpected types")

    if any(var.ndim != 3 for var in out_vars):
        raise ValueError(
            f"f2py returned result of unexpected shape. Got {[var.shape for var in out_vars]}"
        )

    # TODO if dimensions promoted squeeze them out before returning

    return out_vars  # currently returns only 3D arrays


for shape in [(200, 200, 10)]:
    print("Yihawww")
    order = "F"
    sst = xr.DataArray(np.full(shape, 290.0, order=order))
    t_zt = xr.DataArray(np.full(shape, 280.0, order=order))
    hum_zt = xr.DataArray(np.full(shape, 0.001, order=order))
    u_zu = xr.DataArray(np.full(shape, 1.0, order=order))
    v_zu = xr.DataArray(np.full(shape, -1.0, order=order))
    slp = xr.DataArray(np.full(shape, 101000.0, order=order))

    ql, qh, tau_x, tau_y, evap = flux_xr(sst, t_zt, hum_zt, u_zu, v_zu, slp)

    print(f"ql: {ql.shape}")
    print(f"qh: {qh.shape}")
    print(f"tau_x: {tau_x.shape}")
    print(f"tau_y: {tau_y.shape}")
    print(f"evap: {evap.shape}")
