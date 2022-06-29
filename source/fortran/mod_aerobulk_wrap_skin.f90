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

    nb_iter = Niter
    !! All the variables that are set in INIT
    ctype_humidity = 'sh'
    l_use_skin_schemes = .TRUE.

    CALL AEROBULK_COMPUTE(1, calgo, zt, zu, sst(:, :, 1), t_zt(:, :, 1), &
      &                  hum_zt(:, :, 1), U_zu(:, :, 1), V_zu(:, :, 1), slp(:, :, 1),  &
      &                  QL(:, :, 1), QH(:, :, 1), Tau_x(:, :, 1), Tau_y(:, :, 1),     &
      &                  rad_sw=rad_sw(:, :, 1), rad_lw=rad_lw(:, :, 1), T_s=T_s(:, :, 1), Evp=Evap(:, :, 1))

  END SUBROUTINE AEROBULK_MODEL_SKIN

END MODULE mod_aerobulk_wrapper_skin
