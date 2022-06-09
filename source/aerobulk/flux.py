import aerobulk.aerobulk.mod_aerobulk_wrap as aero
import xarray as xr


def _check_shape(arr):
    if arr.ndim != 3:
        raise ValueError


VALID_ALGOS = ["coare3p0", "coare3p6", "ecmwf", "ncar", "andreas"]
VALID_ALGOS_SKIN = ["coare3p0", "coare3p6", "ecmwf"]


def _check_algo(algo, valids):
    if algo not in valids:
        raise ValueError(f"Algorithm {algo} not valid. Choose from {valids}.")


def noskin_np(
    sst,
    t_zt,
    hum_zt,
    u_zu,
    v_zu,
    slp,
    algo,
    zt,
    zu,
    niter,
):
    """Python wrapper for aerobulk without skin correction.
    !ATTENTION If input not provided in correct units, will crash.

    Parameters
    ----------
    sst : array_like
        Bulk sea surface temperature [K]
    t_zt : array_like
        Absolute air temperature at height zt [K]
    hum_zt : array_like
        air humidity at zt, can be given as:
            - specific humidity                   [kg/kg]
            - dew-point temperature                   [K]
            - relative humidity                       [%]
            => type should normally be recognized based on value range
    u_zu : array_like
        zonal wind speed at zu                    [m/s]
    v_zu : array_like
        meridional wind speed at zu               [m/s]
    slp : array_like, optional
        mean sea-level pressure                   [Pa] ~101000 Pa,
        by default 101000.0
    algo : str, optional
        Algorithm, can be one of: "coare3p0", "coare3p6", "ecmwf", "ncar", "andreas",
        by default "coare3p0"
    zt : int, optional
        height for temperature and spec. hum. of air           [m],
        by default 10
    zu : int, optional
        height for wind (10m = traditional anemometric height  [m],
        by default 2
    niter : int, optional
        Number of iteration steps used in the algorithm,
        by default 1

    Returns
    -------
    ql : array_like
        Latent heat flux                                     [W/m^2]
    qh : array_like
        Sensible heat flux                                   [W/m^2]
    taux : array_like
        zonal wind stress                                    [N/m^2]
    tauy : array_like
        meridional wind stress                                    [N/m^2]
    evap : array_like
        evaporation         [mm/s] aka [kg/m^2/s] (usually <0, as ocean loses water!)
    """

    # TODO check if removing default Nones here still passes kwargs down

    for arg in [sst, t_zt, hum_zt, u_zu, v_zu, slp]:
        _check_shape(arg)

    ql, qh, taux, tauy, evap = aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, niter
    )
    return ql, qh, taux, tauy, evap


def skin_np(
    sst,
    t_zt,
    hum_zt,
    u_zu,
    v_zu,
    rad_sw,
    rad_lw,
    slp,
    algo,
    zt,
    zu,
    niter,
    # slp=101000.0,
    # algo="coare3p0",
    # zt=10,
    # zu=2,
    # niter=1,
):
    """Python wrapper for aerobulk with skin correction.
    !ATTENTION If input not provided in correct units, will crash.

    Parameters
    ----------
    sst : array_like
        Bulk sea surface temperature [K]
    t_zt : array_like
        Absolute air temperature at height zt [K]
    hum_zt : array_like
        air humidity at zt, can be given as:
            - specific humidity                   [kg/kg]
            - dew-point temperature                   [K]
            - relative humidity                       [%]
            => type should normally be recognized based on value range
    u_zu : array_like
        zonal wind speed at zu                    [m/s]
    v_zu : array_like
        meridional wind speed at zu               [m/s]
    rad_sw : array_like
        downwelling shortwave radiation at the surface (>0)   [W/m^2]
    rad_lw : array_like
        rad_lw : downwelling longwave radiation at the surface  (>0)   [W/m^2]
    slp : array_like, optional
        mean sea-level pressure                   [Pa] ~101000 Pa,
        by default 101000.0
    algo : str, optional
        Algorithm, can be one of: "coare3p0", "coare3p6", "ecmwf",
        by default "coare3p0"
    zt : int, optional
        height for temperature and spec. hum. of air           [m],
        by default 10
    zu : int, optional
        height for wind (10m = traditional anemometric height  [m],
        by default 2
    niter : int, optional
        Number of iteration steps used in the algorithm,
        by default 1

    Returns
    -------
    ql : array_like
        Latent heat flux                                     [W/m^2]
    qh : array_like
        Sensible heat flux                                   [W/m^2]
    taux : array_like
        zonal wind stress                                    [N/m^2]
    tauy : array_like
        meridional wind stress                                    [N/m^2]
    t_s : array_like
        skin temperature    [K]    (only when l_use_skin_schemes=TRUE)
    evap : array_like
        evaporation         [mm/s] aka [kg/m^2/s] (usually <0, as ocean loses water!)
    """

    ql, qh, taux, tauy, t_s, evap = aero.mod_aerobulk_wrapper.aerobulk_model_skin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, rad_sw, rad_lw, niter
    )
    return ql, qh, taux, tauy, t_s, evap


def noskin(
    sst, t_zt, hum_zt, u_zu, v_zu, slp=101000.0, algo="coare3p0", zt=10, zu=2, niter=6
):
    _check_algo(algo, VALID_ALGOS)

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
        noskin_np,
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        slp,
        input_core_dims=[()] * 6,
        output_core_dims=[()] * 5,
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


def skin(
    sst,
    t_zt,
    hum_zt,
    u_zu,
    v_zu,
    rad_sw,
    rad_lw,
    slp=101000.0,
    algo="coare3p0",
    zt=10,
    zu=2,
    niter=6,
):

    _check_algo(algo, VALID_ALGOS_SKIN)

    sst, t_zt, hum_zt, u_zu, v_zu, slp, rad_sw, rad_lw = xr.broadcast(
        sst, t_zt, hum_zt, u_zu, v_zu, slp, rad_sw, rad_lw
    )

    if len(sst.dims) < 3:
        # TODO promote using expand_dims?
        raise NotImplementedError
    if len(sst.dims) > 4:
        # TODO iterate over extra dims? Or reshape?
        raise NotImplementedError

    out_vars = xr.apply_ufunc(
        skin_np,
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        rad_sw,
        rad_lw,
        slp,
        input_core_dims=[()] * 8,
        output_core_dims=[()] * 6,
        dask="parallelized",
        kwargs=dict(
            algo=algo,
            zt=zt,
            zu=zu,
            niter=niter,
        ),
        output_dtypes=[sst.dtype]
        * 6,  # deactivates the 1 element check which aerobulk does not like
    )

    if not isinstance(out_vars, tuple) or len(out_vars) != 5:
        raise TypeError("F2Py returned unexpected types")

    if any(var.ndim != 3 for var in out_vars):
        raise ValueError(
            f"f2py returned result of unexpected shape. Got {[var.shape for var in out_vars]}"
        )

    # TODO if dimensions promoted squeeze them out before returning

    return out_vars  # currently returns only 3D arrays
