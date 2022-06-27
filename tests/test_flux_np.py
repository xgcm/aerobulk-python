from typing import Dict, Tuple

import numpy as np
import pytest
from aerobulk.flux import noskin_np, skin_np
from numpy.random import default_rng


def create_data_np(
    shape: Tuple[int, ...],
    chunks: Dict[str, int] = {},
    skin_correction: bool = False,
    order: str = "F",
):
    size = shape[0] * shape[1]
    shape2d = (shape[0], shape[1])
    rng = default_rng()
    indices = rng.choice(size, size=int(size * 0.3), replace=False)
    multi_indices = np.unravel_index(indices, shape2d)

    def _arr(value, chunks, order, land_mask=False):
        raw_data = np.full(shape, value, order=order)
        if land_mask:
            raw_data[
                multi_indices[0], multi_indices[1], :
            ] = np.nan  # add NaNs to mimic land mask
        arr = raw_data

        # adds random noise scaled by a percentage of the value
        randomize_factor = 0.001
        randomize_range = value * randomize_factor
        noise = np.random.rand(*shape) * randomize_range
        arr = arr + noise
        return arr

    sst = _arr(290.0, chunks, order, land_mask=True)
    t_zt = _arr(280.0, chunks, order)
    hum_zt = _arr(0.001, chunks, order)
    u_zu = _arr(1.0, chunks, order)
    v_zu = _arr(-1.0, chunks, order)
    slp = _arr(101000.0, chunks, order)
    rad_sw = _arr(0.000001, chunks, order)
    rad_lw = _arr(350.0, chunks, order)
    if skin_correction:
        return sst, t_zt, hum_zt, u_zu, v_zu, rad_sw, rad_lw, slp
    else:
        return sst, t_zt, hum_zt, u_zu, v_zu, slp


@pytest.mark.parametrize("skin_correction", [True, False])
def test_land_mask(skin_correction):
    shape = (2, 3, 4)
    size = shape[0] * shape[1] * shape[2]

    if skin_correction:
        func = skin_np
    else:
        func = noskin_np

    args = create_data_np(shape, chunks=False, skin_correction=skin_correction)
    out_data = func(*args, "ecmwf", 2, 10, 6)

    # Check the location of all NaNs is correct
    for o in out_data:
        np.testing.assert_allclose(np.isnan(args[0]), np.isnan(o))

    # Check that values of the unshrunk array are correct
    for i in range(size):
        index = np.unravel_index(i, shape)
        if not np.isnan(out_data[0][index]):
            single_inputs = tuple(np.atleast_3d(i[index]) for i in args)

            single_outputs = func(*single_inputs, "ecmwf", 2, 10, 6)
            for so, o in zip(single_outputs, out_data):
                assert so == o[index]
