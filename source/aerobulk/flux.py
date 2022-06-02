import aerobulk.aerobulk.mod_aerobulk_wrap as aero

VALID_ALGOS = ["coare3p0", "coare3p6", "ecmwf", "ncar", "andreas"]
VALID_ALGOS_SKIN = ["coare3p0", "coare3p6", "ecmwf"]


def _check_algo(algo, valids):
    if algo not in valids:
        raise ValueError(f"Algorithm {algo} not valid. Choose from {valids}.")


def flux_noskin(
    sst, t_zt, hum_zt, u_zu, v_zu, slp=101000.0, algo="coare3p0", zt=10, zu=2, niter=1
):
    _check_algo(algo, VALID_ALGOS)
    return aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, niter
    )


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
    _check_algo(algo, VALID_ALGOS_SKIN)
    return aero.mod_aerobulk_wrapper.aerobulk_model_skin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, rad_sw, rad_lw, niter
    )
