import aerobulk.aerobulk.mod_aerobulk_wrap as aero

VALID_ALGOS = ["coare3p0", "coare3p6", "ecmwf", "ncar", "andreas"]
VALID_ALGOS_SKIN = ["coare3p0", "coare3p6", "ecmwf"]


def _check_algo(algo, valids):
    if algo not in valids:
        raise ValueError(f"Algorithm {algo} not valid. Choose from {valids}.")


def flux_noskin(
    sst, t_zt, hum_zt, u_zu, v_zu, slp=101000.0, algo="coare3p0", zt=10, zu=2, niter=1
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
    _check_algo(algo, VALID_ALGOS)
    ql, qh, taux, tauy, evap = aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, niter
    )
    return ql, qh, taux, tauy, evap


def flux_skin(
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
    niter=1,
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
    _check_algo(algo, VALID_ALGOS_SKIN)

    ql, qh, taux, tauy, t_s, evap = aero.mod_aerobulk_wrapper.aerobulk_model_skin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, rad_sw, rad_lw, niter
    )
    return ql, qh, taux, tauy, t_s, evap
