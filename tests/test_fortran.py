import numpy as np
import pytest
from aerobulk.flux import noskin_np, skin_np


@pytest.mark.parametrize(
    "algo, expected",
    [
        (
            "andreas",
            {
                "ql": -74.4637756,
                "qh": -14.4129944,
                "evap": -2.62721038,
                "taux": 3.02770734e-02,
                "tauy": 0.0,
            },
        ),
        (
            "ncar",
            {
                "ql": -88.4781876,
                "qh": -16.6969528,
                "evap": -3.12166286,
                "taux": 3.58519591e-02,
                "tauy": 0.0,
            },
        ),
    ],
)
def test_fortran_noskin(algo, expected):
    # Compare python wrapper output with fortran results from (...)

    # inputs are the same for all test cases in the fortran example
    rt0 = np.atleast_3d(273.15)  # conversion to Kelvin
    sst = np.atleast_3d(rt0 + 22)  # in Kelvin
    t_zt = np.atleast_3d(rt0 + 20)
    hum_zt = np.atleast_3d(0.012)  # kg/kg
    u_zu = np.atleast_3d(5)  # m/s
    v_zu = np.atleast_3d(0)
    slp = np.atleast_3d(101000.0)  # Pa
    ql, qh, taux, tauy, evap = noskin_np(
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        slp,
        algo=algo,
        zt=2,
        zu=10,
        niter=10,
        input_range_check=True,
    )
    evap = evap * 3600 * 24  # convert to mm/day

    np.testing.assert_allclose(ql, expected["ql"])
    np.testing.assert_allclose(qh, expected["qh"])
    np.testing.assert_allclose(evap, expected["evap"])
    np.testing.assert_allclose(taux, expected["taux"])
    np.testing.assert_allclose(tauy, expected["tauy"])


@pytest.mark.parametrize(
    "algo, expected",
    [
        (
            "coare3p0",
            {
                "ql": -81.3846741,
                "qh": -15.1545086,
                "evap": -2.87061906,
                "taux": 3.57834995e-02,
                "tauy": 0.0,
                "t_s": 21.7219677,
            },
        ),
        (
            "coare3p6",
            {
                "ql": -83.0788422,
                "qh": -15.3865499,
                "evap": -2.93033028,
                "taux": 3.21817845e-02,
                "tauy": 0.0,
                "t_s": 21.7057972,
            },
        ),
        (
            "ecmwf",
            {
                "ql": -80.2958984,
                "qh": -14.3822346,
                "evap": -2.83224440,
                "taux": 3.84389125e-02,
                "tauy": 0.0,
                "t_s": 21.7325401,
            },
        ),
    ],
)
def test_fortran_skin(algo, expected):
    # Compare python wrapper output with fortran results from (...)

    # inputs are the same for all test cases in the fortran example
    rt0 = np.atleast_3d(273.15)  # conversion to Kelvin
    sst = np.atleast_3d(rt0 + 22)  # in Kelvin
    t_zt = np.atleast_3d(rt0 + 20)
    hum_zt = np.atleast_3d(0.012)  # kg/kg
    u_zu = np.atleast_3d(5)  # m/s
    v_zu = np.atleast_3d(0)
    slp = np.atleast_3d(101000.0)  # Pa
    rad_sw = np.atleast_3d(0)
    rad_lw = np.atleast_3d(350)
    ql, qh, taux, tauy, t_s, evap = skin_np(
        sst,
        t_zt,
        hum_zt,
        u_zu,
        v_zu,
        slp,
        rad_sw,
        rad_lw,
        algo=algo,
        zt=2,
        zu=10,
        niter=10,
        input_range_check=True,
    )
    evap = evap * 3600 * 24  # convert to mm/day
    t_s = t_s - rt0

    np.testing.assert_allclose(ql, expected["ql"])
    np.testing.assert_allclose(qh, expected["qh"])
    np.testing.assert_allclose(evap, expected["evap"])
    np.testing.assert_allclose(taux, expected["taux"])
    np.testing.assert_allclose(tauy, expected["tauy"])
    np.testing.assert_allclose(t_s, expected["t_s"])
