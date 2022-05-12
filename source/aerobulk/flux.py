import aerobulk.aerobulk.mod_aerobulk_wrap as aero


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
