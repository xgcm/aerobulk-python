import pytest
import numpy as np
import xarray as xr
from aerobulk import flux_noskin


def test_smoke_test():
    # I am very confused about shape.
    # We want Nt to be the *slowest varying dimension*
    # For C-order arrays, would give (Nt, Ny, Nx)
    # For F-order arrays, would give (Nx, Ny, Nt)
    # I expected that the shape passed from Python would reverse when it hits Fortran,
    # but this did not happen.
    shape = (200, 200, 10)
    order = "F"
    sst = np.full(shape, 290.0, order=order)
    t_zt = np.full(shape, 280.0, order=order)
    hum_zt = np.full(shape, 0.001, order=order)
    u_zu = np.full(shape, 1.0, order=order)
    v_zu = np.full(shape, -1.0, order=order)
    slp = np.full(shape, 101000.0, order=order)
    flux_noskin(sst, t_zt, hum_zt, u_zu, v_zu, slp)


def test_dataset(chunks=None):
    # this ensures that transposing the input does not affect the output results
    def create_arr(value):
        data = np.full((2, 4, 3), value)
        # insert nans at differnt locations
        data_missing = data.flatten()
        # data_missing[np.array([3, 12, 22])] = np.nan
        data_missing = np.reshape(data_missing, data.shape)
        out = xr.DataArray(data_missing, dims=["x", "y", "time"])
        return out

    ds = xr.Dataset(
        {
            "sst": create_arr(290.0),
            "t_zt": create_arr(280.0),
            "hum_zt": create_arr(0.001),
            "u_zu": create_arr(1.0),
            "v_zu": create_arr(-1.0),
            "slp": create_arr(101000.0),
        }
    )
    if chunks:
        ds = ds.chunk(chunks)
    return ds


def test_shape_invariance():
    ds = test_dataset()
    print(ds)
    results = flux_noskin(
        ds.sst.data,
        ds.t_zt.data,
        ds.hum_zt.data,
        ds.u_zu.data,
        ds.v_zu.data,
        ds.slp.data,
    )

    # ds_transposed = ds.transpose(("x", "time", "y"))
    # results_transposed = flux_noskin(
    #     ds_transposed.sst,
    #     ds_transposed.t_zt,
    #     ds_transposed.hum_zt,
    #     ds_transposed.u_zu,
    #     ds_transposed.v_zu,
    #     ds_transposed.slp,
    # )
    # print(results[0], results[0].shape)

    print(results_transposed[0], results_transposed[0].shape)
    assert 1 == 0


@pytest.mark.parametrize("chunks", [{}, {"time": 1}])
def test_apply_ufunc(chunks):
    # Check output over various chunking schemes
    #
    def create_arr(value):
        return xr.DataArray(np.full((2, 4, 3), value), dims=["x", "y", "time"]).chunk(
            chunks
        )

    # create dummy xarray dataset
    sst = create_arr(290)
    t_zt = create_arr(280)
    hum_zt = create_arr(0.001)
    u_zu = create_arr(1.0)
    v_zu = create_arr(-1.0)
    slp = create_arr(101000.0)
    # expected values
    (
        ql_expected,
        qh_expected,
        tau_x_expected,
        tau_y_expected,
        evap_expected,
    ) = flux_noskin(sst, t_zt, hum_zt, u_zu, v_zu, slp)

    # computed with apply_ufunc
    cd = [
        "x",
        "y",
    ]  # this restricts the problem somewhat (generally we should be able to parallelize as)
    ql, qh, tau_x, tau_y, evap = xr.apply_ufunc(
        flux_noskin,
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        slp,
        dask="parallelized",
        input_core_dimensions=[cd, cd, cd, cd, cd, cd],
    )

    print(ql)
    assert 1 == 0


# @pytest.mark.xfail(strict=True, reason="WIP")
# def test_aerobulk_toy_results():
#     # This test aims to check the python API against the results posted in the README of
#     # aerobulk (https://github.com/brodeau/aerobulk#-giving-aerobulk-a-first-try-in-interactive-toy-mode)
#     niter = 20
#     zu = 10
#     zt = 2
#     sst = 22 + 273  # 22 C
#     t_zt = 20 + 273  # 20 C
#     hum_zt = 0.012  # 12 g/kg
#     u_zu = 5
#     v_zu = 0

#     # start with only one algo
#     # algo = "coare3p0"
#     algo = "ncar"
#     expected_tau_x = 36.198
#     expected_evap = 3.1059
#     expected_ql = -88.031
#     expected_qh = -17.833

#     ql, qh, tau_x, tau_y, evap = flux_noskin(
#         sst,
#         t_zt,
#         hum_zt,
#         u_zu,
#         v_zu,
#         zu=zu,
#         zt=zt,
#         niter=niter,
#     )
#     # convert to the units displayed...geez this is convoluted...
#     evap = 3600 * 24 * evap
#     tau_x = 1e3 * tau_x

#     print(tau_x, evap, ql, qh)
#     assert 1 == 0
