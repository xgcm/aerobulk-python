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
    out_data = func(*args, algo, 2, 10, 6, input_range_check=True)

    # Check the location of all NaNs is correct
    for o in out_data:
        np.testing.assert_allclose(np.isnan(args[0]), np.isnan(o))

    # Check that values of the unshrunk array are correct
    for i in range(size):
        index = np.unravel_index(i, shape)
        if not np.isnan(out_data[0][index]):
            single_inputs = tuple(np.atleast_3d(i[index]) for i in args)

            single_outputs = func(
                *single_inputs, algo, 2, 10, 6, input_range_check=True
            )
            for so, o in zip(single_outputs, out_data):
                assert so == o[index]


@pytest.mark.parametrize(
    "varname",
    [
        "t_zt",
        "hum_zt",
        "u_zu",
        "v_zu",
        "slp",
        "rad_sw",
        "rad_lw",
    ],
)
class Test_Range_Check:
    def test_range_check_nan(self, varname):
        shape = (1, 1)
        args_noskin = create_data(shape, skin_correction=False, **{varname: np.nan})
        args_skin = create_data(shape, skin_correction=True, **{varname: np.nan})
        msg = f"Found nans in {varname} that do not align with nans in `sst`. Check that nans in all fields are matched."

        if varname not in ["rad_sw", "rad_lw"]:  # Test these only for skin
            with pytest.raises(ValueError, match=msg):
                noskin_np(*args_noskin, "ecmwf", 2, 10, 6, input_range_check=True)

        with pytest.raises(ValueError, match=msg):
            skin_np(*args_skin, "ecmwf", 2, 10, 6, input_range_check=True)

    def test_range_check_invalid(self, varname):
        invalid_values = {
            "sst": 200.0,
            "t_zt": 100,
            "hum_zt": 0.1,
            "u_zu": 60,
            "v_zu": 60,
            "slp": 2000,
            "rad_sw": -20,
            "rad_lw": 4000,
        }
        shape = (1, 1)
        args_noskin = create_data(
            shape, skin_correction=False, **{varname: invalid_values[varname]}
        )
        args_skin = create_data(
            shape, skin_correction=True, **{varname: invalid_values[varname]}
        )
        # I hate regex sooo much. If someone has a nice way to just test that the varname is in here and the error message contains 'range'
        # that would be amazing. This is the best I could do...
        msg = r"\b(?:out of the valid range)\b"
        if varname not in ["rad_sw", "rad_lw"]:
            with pytest.raises(ValueError, match=str(msg)):
                noskin_np(*args_noskin, "ecmwf", 2, 10, 6, input_range_check=True)

        with pytest.raises(ValueError, match=str(msg)):
            skin_np(*args_skin, "ecmwf", 2, 10, 6, input_range_check=True)
