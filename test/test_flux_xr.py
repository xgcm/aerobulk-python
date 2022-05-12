"""Tests for the xarray wrapper"""
from typing import Dict

import numpy as np
import pytest
import xarray as xr
from aerobulk import flux_noskin_xr


def create_data(shape: tuple, chunks: Dict[str, int] = {}):
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
    return sst, t_zt, hum_zt, u_zu, v_zu, slp


@pytest.mark.parametrize("chunks", [{"dim_2": -1}, {"dim_0": 10}, {"dim_2": 10}])
def test_chunked(chunks):
    shape = (50, 50, 20)
    args = create_data(shape)
    print(args)
    out_vars_chunked = flux_noskin_xr(*args)
    out_vars_nochunks = flux_noskin_xr(*(a.load() for a in args))

    for out_chunk, out_nochunk in zip(out_vars_chunked, out_vars_nochunks):
        assert out_chunk.shape == shape
        xr.testing.assert_allclose(out_chunk, out_nochunk)


@pytest.mark.parametrize("chunks", [{"dim_2": -1}, {"dim_0": 10}, {"dim_2": 10}])
def test_transpose_invariance(chunks):
    shape = (50, 50, 20)
    args = create_data(shape)
    out_vars_012 = flux_noskin_xr(
        *(a.transpose("dim_0", "dim_1", "dim_2") for a in args)
    )
    out_vars_210 = flux_noskin_xr(
        *(a.transpose("dim_2", "dim_1", "dim_0") for a in args)
    )
    out_vars_120 = flux_noskin_xr(
        *(a.transpose("dim_1", "dim_2", "dim_0") for a in args)
    )

    for i, ii, iii in zip(out_vars_012, out_vars_120, out_vars_210):
        xr.testing.assert_allclose(
            i.transpose("dim_0", "dim_1", "dim_2"),
            ii.transpose("dim_0", "dim_1", "dim_2"),
        )
        xr.testing.assert_allclose(
            ii.transpose("dim_0", "dim_1", "dim_2"),
            iii.transpose("dim_0", "dim_1", "dim_2"),
        )
