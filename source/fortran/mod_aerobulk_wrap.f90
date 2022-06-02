MODULE mod_aerobulk_wrapper

  USE mod_const
  USE mod_aerobulk, ONLY: AEROBULK_INIT, AEROBULK_BYE
  USE mod_aerobulk_compute

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: AEROBULK_MODEL_NOSKIN, AEROBULK_MODEL_SKIN

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

    !! initialize based on first timestep
    CALL AEROBULK_INIT(Nt, calgo, sst(:, :, 1), t_zt(:, :, 1), &
        &              hum_zt(:, :, 1), U_zu(:, :, 1), V_zu(:, :, 1), slp(:, :, 1), &
        &              l_use_skin=.FALSE. )

    DO n = 1, Nt
       CALL AEROBULK_COMPUTE(n, calgo, zt, zu, sst(:, :, n), t_zt(:, :, n), &
          &                  hum_zt(:, :, n), U_zu(:, :, n), V_zu(:, :, n), slp(:, :, n),  &
          &                  QL(:, :, n), QH(:, :, n), Tau_x(:, :, n), Tau_y(:, :, n),     &
          &                  Evp=Evap(:, :, n))
    END DO

    CALL AEROBULK_BYE()

  END SUBROUTINE AEROBULK_MODEL_NOSKIN

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

    !! initialize based on first timestep
    CALL AEROBULK_INIT(Nt, calgo, sst(:, :, 1), t_zt(:, :, 1), &
        &              hum_zt(:, :, 1), U_zu(:, :, 1), V_zu(:, :, 1), slp(:, :, 1), &
        &              l_use_skin=.TRUE., prsw=rad_sw(:, :, 1), prlw=rad_lw(:, :, 1))

    DO n = 1, Nt
       CALL AEROBULK_COMPUTE(n, calgo, zt, zu, sst(:, :, n), t_zt(:, :, n), &
          &                  hum_zt(:, :, n), U_zu(:, :, n), V_zu(:, :, n), slp(:, :, n),  &
          &                  QL(:, :, n), QH(:, :, n), Tau_x(:, :, n), Tau_y(:, :, n),     &
          &                  rad_sw=rad_sw(:, :, n), rad_lw=rad_lw(:, :, n), T_s=T_s(:, :, n), Evp=Evap(:, :, n))
    END DO

    CALL AEROBULK_BYE()

  END SUBROUTINE AEROBULK_MODEL_SKIN

END MODULE mod_aerobulk_wrapper
