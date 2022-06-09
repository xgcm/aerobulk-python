"""Tests for the xarray wrapper"""
from typing import Dict

import numpy as np
import pytest
import xarray as xr
from aerobulk import noskin, skin
from aerobulk.flux import VALID_ALGOS, VALID_ALGOS_SKIN


def create_data(shape: tuple, chunks: Dict[str, int] = {}, skin_correction=False):
    def _arr(value):
        order = "F"  # Does this matter for the results?
        arr = xr.DataArray(np.full(shape, value, order=order))

        # adds random noise scaled by a percentage of the value
        randomize_factor = 0.001
        randomize_range = value * randomize_factor
        arr = arr + np.random.rand(*shape) + randomize_range

        if chunks:
            arr = arr.chunk(chunks)
        return arr

    sst = _arr(290.0)
    t_zt = _arr(280.0)
    hum_zt = _arr(0.001)
    u_zu = _arr(1.0)
    v_zu = _arr(-1.0)
    slp = _arr(101000.0)
    rad_sw = _arr(0)
    rad_lw = _arr(350)
    if skin_correction:
        return sst, t_zt, hum_zt, u_zu, v_zu, slp, rad_sw, rad_lw
    else:
        return sst, t_zt, hum_zt, u_zu, v_zu, slp


@pytest.mark.parametrize(
    "algo",
    VALID_ALGOS
    + [pytest.param("wrong", marks=pytest.mark.xfail(strict=True, raises=ValueError))],
)
def test_algo_error_noskin(algo):
    shape = (1, 1, 1)
    args = create_data(shape, chunks=False, skin_correction=False)
    noskin(*args, algo=algo)


@pytest.mark.parametrize(
    "algo",
    VALID_ALGOS_SKIN
    + [pytest.param("wrong", marks=pytest.mark.xfail(strict=True, raises=ValueError))],
)
def test_algo_error_skin(algo):
    shape = (1, 1, 1)
    args = create_data(shape, chunks=False, skin_correction=True)
    skin(*args, algo=algo)


@pytest.mark.parametrize("skin_correction", [True, False])
@pytest.mark.parametrize("chunks", [{"dim_2": -1}, {"dim_0": 10}, {"dim_2": 10}])
class Test_xarray_wrapper:
    def test_chunked(self, chunks, skin_correction):
        shape = (50, 50, 20)
        args = create_data(shape, chunks=chunks, skin_correction=skin_correction)
        if skin_correction:
            func = skin
        else:
            func = noskin
        out_vars_chunked = func(*args)
        out_vars_nochunks = func(*(a.load() for a in args))

        for out_chunk, out_nochunk in zip(out_vars_chunked, out_vars_nochunks):
            assert out_chunk.shape == shape
            xr.testing.assert_allclose(out_chunk, out_nochunk)

    def test_transpose_invariance(self, chunks, skin_correction):
        shape = (50, 50, 20)
        args = create_data(shape, chunks=chunks, skin_correction=skin_correction)
        if skin_correction:
            func = skin
        else:
            func = noskin
        out_vars_012 = func(*(a.transpose("dim_0", "dim_1", "dim_2") for a in args))
        out_vars_210 = func(*(a.transpose("dim_2", "dim_1", "dim_0") for a in args))
        out_vars_120 = func(*(a.transpose("dim_1", "dim_2", "dim_0") for a in args))

        for i, ii, iii in zip(out_vars_012, out_vars_120, out_vars_210):
            xr.testing.assert_allclose(
                i.transpose("dim_0", "dim_1", "dim_2"),
                ii.transpose("dim_0", "dim_1", "dim_2"),
            )
            xr.testing.assert_allclose(
                ii.transpose("dim_0", "dim_1", "dim_2"),
                iii.transpose("dim_0", "dim_1", "dim_2"),
            )
