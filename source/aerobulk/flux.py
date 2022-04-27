import aerobulk.aerobulk.mod_aerobulk_wrap as aero


def flux_noskin(
    sst, t_zt, hum_zt, u_zu, v_zu, slp, calgo="coare3p0", zt=10, zu=2, niter=1
):
    return aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        calgo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, niter
    )
