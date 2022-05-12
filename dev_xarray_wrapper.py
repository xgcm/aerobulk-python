import numpy as np
import xarray as xr
from aerobulk import flux_noskin


def flux_xr(
    sst, t_zt, hum_zt, u_zu, v_zu, slp=101000.0, algo="coare3p0", zt=10, zu=2, niter=1
):
    # TODO do we need to make the "time" dimension special?

    sst, t_zt, hum_zt, u_zu, v_zu, slp = xr.broadcast(
        sst, t_zt, hum_zt, u_zu, v_zu, slp
    )
    print("slp", slp)

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
        # input_core_dims=[("dim_0", "dim_1", "dim_2")] * 6,
        # output_core_dims=[("dim_0", "dim_1", "dim_2")] * 5,
        dask="parallelized",
        kwargs=dict(
            algo=algo,
            zt=zt,
            zu=zu,
            niter=niter,
        ),
        output_dtypes=[sst.dtype]
        * 5,  # deactivates the 1 element check which aerobulk does not like
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
    sst = xr.DataArray(np.full(shape, 290.0, order=order)).chunk(
        {"dim_0": 10, "dim_2": 10}
    )
    t_zt = xr.DataArray(np.full(shape, 280.0, order=order)).chunk(
        {"dim_0": 10, "dim_2": 10}
    )
    hum_zt = xr.DataArray(np.full(shape, 0.001, order=order)).chunk(
        {"dim_0": 10, "dim_2": 10}
    )
    u_zu = xr.DataArray(np.full(shape, 1.0, order=order)).chunk(
        {"dim_0": 10, "dim_2": 10}
    )
    v_zu = xr.DataArray(np.full(shape, -1.0, order=order)).chunk(
        {"dim_0": 10, "dim_2": 10}
    )
    slp = xr.DataArray(np.full(shape, 101000.0, order=order)).chunk(
        {"dim_0": 10, "dim_2": 10}
    )
    print(slp.chunks)

    ql, qh, tau_x, tau_y, evap = flux_xr(sst, t_zt, hum_zt, u_zu, v_zu, slp)

    print(f"ql: {ql.shape}")
    print(f"qh: {qh.shape}")
    print(f"tau_x: {tau_x.shape}")
    print(f"tau_y: {tau_y.shape}")
    print(f"evap: {evap.shape}")

    print(f"ql: {ql.load()}")
    print(f"qh: {qh.load()}")
    print(f"tau_x: {tau_x.load()}")
    print(f"tau_y: {tau_y.load()}")
    print(f"evap: {evap.load()}")
