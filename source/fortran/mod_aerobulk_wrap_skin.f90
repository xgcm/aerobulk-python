MODULE mod_aerobulk_wrapper_skin

  USE mod_const
  USE mod_aerobulk, ONLY: AEROBULK_INIT, AEROBULK_BYE
  USE mod_aerobulk_compute

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: AEROBULK_MODEL_SKIN

CONTAINS
  SUBROUTINE AEROBULK_MODEL_SKIN( Ni, Nj, Nt,          &
     &                       calgo, zt, zu, sst, t_zt,   &
     &                       hum_zt, U_zu, V_zu, slp,    &
     &                       rad_sw, rad_lw,             &
     &                       Niter,                      &
     &                       QL, QH, Tau_x, Tau_y, T_s, Evap)

    INTEGER,                  INTENT(in)  :: Ni, Nj, Nt
    CHARACTER(len=*),         INTENT(in)  :: calgo
    REAL(8),                 INTENT(in)  :: zt, zu
    REAL(8), DIMENSION(Ni,Nj,Nt), INTENT(in)  :: sst, t_zt, hum_zt, U_zu, V_zu, slp, rad_sw, rad_lw
    INTEGER,                  INTENT(in) :: Niter
    REAL(8), DIMENSION(Ni,Nj,Nt), INTENT(out) :: QL, QH, Tau_x, Tau_y, T_s, Evap

    INTEGER n
    nb_iter = Niter
    !! All the variables that are set in INIT
    ctype_humidity = 'sh'
    l_use_skin_schemes = .FALSE.
    DO n = 1, Nt
       CALL AEROBULK_COMPUTE(n, calgo, zt, zu, sst(:, :, n), t_zt(:, :, n), &
          &                  hum_zt(:, :, n), U_zu(:, :, n), V_zu(:, :, n), slp(:, :, n),  &
          &                  QL(:, :, n), QH(:, :, n), Tau_x(:, :, n), Tau_y(:, :, n),     &
          &                  rad_sw=rad_sw(:, :, n), rad_lw=rad_lw(:, :, n), T_s=T_s(:, :, n), Evp=Evap(:, :, n))
    END DO

    CALL AEROBULK_BYE()

  END SUBROUTINE AEROBULK_MODEL_SKIN

END MODULE mod_aerobulk_wrapper_skin
