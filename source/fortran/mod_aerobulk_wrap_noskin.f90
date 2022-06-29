MODULE mod_aerobulk_wrapper_noskin

  USE mod_const
  USE mod_aerobulk, ONLY: AEROBULK_INIT, AEROBULK_BYE
  USE mod_aerobulk_compute

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: AEROBULK_MODEL_NOSKIN

CONTAINS

  SUBROUTINE AEROBULK_MODEL_NOSKIN( Ni, Nj, Nt,          &
     &                       calgo, zt, zu, sst, t_zt,   &
     &                       hum_zt, U_zu, V_zu, slp,    &
     &                       Niter,                      &
     &                       QL, QH, Tau_x, Tau_y, Evap)

    INTEGER,                  INTENT(in)  :: Ni, Nj, Nt
    CHARACTER(len=*),         INTENT(in)  :: calgo
    REAL(8),                 INTENT(in)  :: zt, zu
    REAL(8), DIMENSION(Ni,Nj,Nt), INTENT(in)  :: sst, t_zt, hum_zt, U_zu, V_zu, slp
    INTEGER,                  INTENT(in) :: Niter
    REAL(8), DIMENSION(Ni,Nj,Nt), INTENT(out) :: QL, QH, Tau_x, Tau_y, Evap

    INTEGER n

    nb_iter = Niter
    !! All the variables that are set in INIT
    ctype_humidity = 'sh'
    l_use_skin_schemes = .FALSE.

    CALL AEROBULK_COMPUTE(n, calgo, zt, zu, sst(:, :, n), t_zt(:, :, n), &
      &                  hum_zt(:, :, n), U_zu(:, :, n), V_zu(:, :, n), slp(:, :, n),  &
      &                  QL(:, :, n), QH(:, :, n), Tau_x(:, :, n), Tau_y(:, :, n),     &
      &                  Evp=Evap(:, :, n))

    CALL AEROBULK_BYE()

  END SUBROUTINE AEROBULK_MODEL_NOSKIN

END MODULE mod_aerobulk_wrapper_noskin
