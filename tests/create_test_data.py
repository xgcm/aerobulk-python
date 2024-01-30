from typing import Dict, Tuple

import numpy as np
import xarray as xr
from numpy.random import default_rng


def create_data(
    shape: Tuple[int, ...],
    chunks: Dict[str, int] = {},
    skin_correction: bool = False,
    order: str = "F",
    use_xr=True,
    land_mask=False,
    sst=290.0,
    t_zt=280.0,
    hum_zt=0.001,
    u_zu=1.0,
    v_zu=-1.0,
    slp=101000.0,
    rad_sw=0.000001,
    rad_lw=350.0,
):
    size = shape[0] * shape[1]
    shape2d = (shape[0], shape[1])
    rng = default_rng()
    indices = rng.choice(size, size=int(size * 0.3), replace=False)
    multi_indices = np.unravel_index(indices, shape2d)

    def _arr(value, chunks, order):
        arr = np.full(shape, value, order=order)
        # adds random noise scaled by a percentage of the value
        randomize_factor = 0.001
        randomize_range = value * randomize_factor
        noise = np.random.rand(*shape) * randomize_range
        arr = arr + noise

        if land_mask:
            arr[multi_indices[0], multi_indices[1], :] = (
                np.nan
            )  # add NaNs to mimic land mask
        if use_xr:
            arr = xr.DataArray(arr)
            if chunks:
                arr = arr.chunk(chunks)
        return arr

    if skin_correction:
        return tuple(
            _arr(a, chunks, order)
            for a in (
                sst,
                t_zt,
                hum_zt,
                u_zu,
                v_zu,
                slp,
                rad_sw,
                rad_lw,
            )
        )
    else:
        return tuple(
            _arr(a, chunks, order) for a in (sst, t_zt, hum_zt, u_zu, v_zu, slp)
        )
