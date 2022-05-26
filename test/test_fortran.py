import numpy as np
import pytest
from aerobulk import flux_noskin


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
def test_fortran_code(algo, expected):
    # Compare python wrapper output with fortran results from (...)

    # inputs are the same for all test cases in the fortran example
    rt0 = 273.15  # conversion to Kelvin
    sst = rt0 + 22  # in Kelvin
    t_zt = rt0 + 20
    hum_zt = 0.012  # kg/kg
    u_zu = 5  # m/s
    v_zu = 0
    slp = 101000.0  # Pa
    ql, qh, taux, tauy, evap = flux_noskin(
        sst, t_zt, hum_zt, u_zu, v_zu, slp, algo=algo, zt=2, zu=10, niter=10
    )
    evap = evap * 3600 * 24  # convert to mm/day

    np.testing.assert_allclose(ql, expected["ql"])
    np.testing.assert_allclose(qh, expected["qh"])
    np.testing.assert_allclose(evap, expected["evap"])
    np.testing.assert_allclose(taux, expected["taux"])
    np.testing.assert_allclose(tauy, expected["tauy"])
