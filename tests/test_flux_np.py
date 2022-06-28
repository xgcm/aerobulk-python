import numpy as np
import pytest
from aerobulk.flux import noskin_np, skin_np
from create_test_data import create_data

"""Tests for the numpy land_mask wrapper"""


@pytest.mark.parametrize(
    "algo, skin_correction",
    [
        ("ecmwf", True),
        ("ecmwf", False),
        ("coare3p0", True),
        ("coare3p0", False),
        ("coare3p6", True),
        ("coare3p6", False),
        ("andreas", False),
        ("ncar", False),
    ],
)
def test_land_mask(skin_correction, algo):
    shape = (2, 3, 4)
    size = shape[0] * shape[1] * shape[2]

    if skin_correction:
        func = skin_np
    else:
        func = noskin_np

    args = create_data(
        shape,
        chunks=False,
        skin_correction=skin_correction,
        use_xr=False,
        land_mask=True,
    )
    out_data = func(*args, algo, 2, 10, 6)

    # Check the location of all NaNs is correct
    for o in out_data:
        np.testing.assert_allclose(np.isnan(args[0]), np.isnan(o))

    # Check that values of the unshrunk array are correct
    for i in range(size):
        index = np.unravel_index(i, shape)
        if not np.isnan(out_data[0][index]):
            single_inputs = tuple(np.atleast_3d(i[index]) for i in args)

            single_outputs = func(*single_inputs, algo, 2, 10, 6)
            for so, o in zip(single_outputs, out_data):
                assert so == o[index]
