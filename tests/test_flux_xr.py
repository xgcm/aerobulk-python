import pytest
import xarray as xr
from aerobulk import noskin, skin
from create_test_data import create_data

"""Tests for the xarray wrapper"""


@pytest.mark.parametrize("algo", ["wrong"])
def test_algo_error_noskin(algo):
    shape = (1, 1, 1)
    args = create_data(shape, chunks=False, skin_correction=False)
    with pytest.raises(ValueError):
        noskin(*args, algo=algo)


@pytest.mark.parametrize("algo", ["wrong", "ncar"])
def test_algo_error_skin(algo):
    shape = (1, 1, 1)
    args = create_data(shape, chunks=False, skin_correction=True)
    with pytest.raises(ValueError):
        skin(*args, algo=algo)


@pytest.mark.parametrize(
    "chunks", [{"dim_2": -1}, {"dim_0": 10}, {"dim_0": 1}, {"dim_2": 8}, {"dim_2": 1}]
)
@pytest.mark.parametrize("skin_correction", [False, True])
class Test_xarray:
    def test_chunked(self, chunks, skin_correction):
        shape = (10, 13, 12)
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

    @pytest.mark.parametrize("order", ["F", "C"])
    def test_transpose_invariance_noskin(self, chunks, skin_correction, order):
        shape = (10, 13, 12)
        chunks = {"dim_0": 10}
        args = create_data(
            shape, chunks=chunks, skin_correction=skin_correction, order=order
        )
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


@pytest.mark.parametrize("skin_correction", [True, False])
def test_all_input_array_sizes_valid(skin_correction):
    shapes = (
        (3, 4),
        (2, 3, 4),
        (2, 3, 4, 5),
    )  # create_data() only allows for inputs of 2 or more dimensions
    data = (create_data(s, skin_correction=skin_correction) for s in shapes)
    if skin_correction:
        func = skin
    else:
        func = noskin
    tuple(func(*d, "coare3p0", 2, 10, 6) for d in data)
    assert (
        1 == 1
    )  # This line is always true, but verifies that the above line doesn't crash the Fortran code
