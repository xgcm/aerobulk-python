import numpy as np
import aerobulk.aerobulk.mod_aerobulk_wrap as aero


def flux_noskin(
    sst, t_zt, hum_zt, u_zu, v_zu, slp=101000.0, algo="coare3p0", zt=10, zu=2, niter=1
):
    # TODO: SLP is optional? seems like that is an option on the original code (https://github.com/brodeau/aerobulk/blob/7cd85ba8d05fd23eb81a8fdd202f11d9b63fa0b7/src/tests/aerobulk_toy.f90#L51)
    # make sure that all inputs are proper 2d (or do we need 3d?) arrays
    # TODO: Is there a way to handle this more clever? Maybe tom has an idea
    # TODO: At least factoring this out might be a good idea, especially once we want to implement flux_skin

    sst = np.atleast_2d(sst)
    t_zt = np.atleast_2d(t_zt)
    hum_zt = np.atleast_2d(hum_zt)
    u_zu = np.atleast_2d(u_zu)
    v_zu = np.atleast_2d(v_zu)
    print(sst)
    print(sst.shape)
    print("----------")

    return aero.mod_aerobulk_wrapper.aerobulk_model_noskin(
        algo, zt, zu, sst, t_zt, hum_zt, u_zu, v_zu, slp, niter
    )
