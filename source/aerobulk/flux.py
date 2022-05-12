import aerobulk.aerobulk.mod_aerobulk_wrap as aero
import xarray as xr


def _check_shape(arr):
    if arr.ndim != 3:
        raise ValueError


def flux_noskin(
    sst,
    t_zt,
    hum_zt,
    u_zu,
    v_zu,
    slp,
    algo=None,
    zt=None,
    zu=None,
    niter=None,
):
    # TODO check if removing default Nones here still passes kwargs down

    for arg in [sst, t_zt, hum_zt, u_zu, v_zu, slp]:
        _check_shape(arg)

    # TODO is niter actually used by our fortran wrapper?

    results = aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, niter
    )

    print([r.shape for r in results])

    return results


def flux_noskin_xr(
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
