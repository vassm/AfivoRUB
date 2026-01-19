#include "../afivo/src/cpp_macros.h"

module m_user
  use m_af_all
  use m_config
  use m_user_methods
  use m_streamer
  use m_field, only: current_voltage

  implicit none
  private

  public :: user_initialize

  ! -------------------------
  ! Electrode geometry
  ! -------------------------
  real(dp) :: user_circle_radius = 1.0e-4_dp

  real(dp) :: user_powered1_rel(NDIM)  = [0.30_dp, 0.54_dp]
  real(dp) :: user_powered2_rel(NDIM)  = [0.60_dp, 0.54_dp]
  real(dp) :: user_grounded1_rel(NDIM) = [0.30_dp, 0.46_dp]
  real(dp) :: user_grounded2_rel(NDIM) = [0.60_dp, 0.46_dp]

  real(dp) :: powered1(NDIM), powered2(NDIM)
  real(dp) :: grounded1(NDIM), grounded2(NDIM)

  ! Small potential offset for the "grounded" electrode (Volts)
  real(dp) :: grounded_phi_offset = 1.0e-3_dp

  ! -------------------------
  ! Dielectric slab
  ! -------------------------
  real(dp) :: dielectric_eps = 5.0_dp
  real(dp) :: slab_y0 = 4.875e-3_dp
  real(dp) :: slab_y1 = 8.125e-3_dp

contains

  subroutine user_initialize(cfg, tree)
    type(CFG_t), intent(inout) :: cfg
    type(af_t),  intent(inout) :: tree

    call CFG_add_get(cfg, "user_circle_radius", user_circle_radius, &
         "Radius (m) of circular electrodes")

    call CFG_add_get(cfg, "user_powered1_rel", user_powered1_rel, "Powered electrode 1 (rel)")
    call CFG_add_get(cfg, "user_powered2_rel", user_powered2_rel, "Powered electrode 2 (rel)")
    call CFG_add_get(cfg, "user_grounded1_rel", user_grounded1_rel, "Grounded electrode 1 (rel)")
    call CFG_add_get(cfg, "user_grounded2_rel", user_grounded2_rel, "Grounded electrode 2 (rel)")

    call CFG_add_get(cfg, "grounded_phi_offset", grounded_phi_offset, &
         "Small offset potential (V) applied to grounded electrode")

    call CFG_add_get(cfg, "dielectric_eps", dielectric_eps, "Dielectric permittivity")
    call CFG_add_get(cfg, "slab_y0", slab_y0, "Dielectric slab lower y (m)")
    call CFG_add_get(cfg, "slab_y1", slab_y1, "Dielectric slab upper y (m)")

    powered1  = ST_domain_origin + user_powered1_rel  * ST_domain_len
    powered2  = ST_domain_origin + user_powered2_rel  * ST_domain_len
    grounded1 = ST_domain_origin + user_grounded1_rel * ST_domain_len
    grounded2 = ST_domain_origin + user_grounded2_rel * ST_domain_len

    user_lsf                => my_user_lsf
    user_lsf_bc             => my_user_lsf_bc
    user_initial_conditions => my_init_cond
  end subroutine user_initialize

  pure real(dp) function circle_lsf(x, c, rad) result(lsf)
    real(dp), intent(in) :: x(NDIM), c(NDIM)
    real(dp), intent(in) :: rad
    lsf = norm2(x - c) - rad
  end function circle_lsf

  real(dp) function my_user_lsf(x) result(lsf)
    real(dp), intent(in) :: x(NDIM)
    lsf = min( &
          min(circle_lsf(x, powered1,  user_circle_radius), &
              circle_lsf(x, powered2,  user_circle_radius)), &
          min(circle_lsf(x, grounded1, user_circle_radius), &
              circle_lsf(x, grounded2, user_circle_radius)) )
  end function my_user_lsf

  real(dp) function my_user_lsf_bc(x) result(phi)
    real(dp), intent(in) :: x(NDIM)
    real(dp) :: mp, mg

    mp = min(circle_lsf(x, powered1, user_circle_radius), &
             circle_lsf(x, powered2, user_circle_radius))
    mg = min(circle_lsf(x, grounded1, user_circle_radius), &
             circle_lsf(x, grounded2, user_circle_radius))

    if (mp < mg) then
      phi = current_voltage
    else
      ! "Grounded" electrode with a tiny offset in Volts
      phi = grounded_phi_offset
    end if
  end function my_user_lsf_bc

subroutine my_init_cond(box)
  type(box_t), intent(inout) :: box
  real(dp) :: y_center
  logical  :: in_slab

  ! If dielectric not enabled (or eps not present), do nothing at all.
  if (.not. ST_use_dielectric) return
  if (i_eps <= 0) return

  ! Box-centered y-coordinate
  y_center = box%r_min(2) + 0.5_dp * box%dr(2) * box%n_cell
  in_slab  = (y_center >= slab_y0 .and. y_center < slab_y1)

  if (in_slab) then
    ! Set dielectric permittivity
    box%cc(:, :, i_eps) = dielectric_eps

    ! Prevent plasma inside the dielectric ONLY
    if (i_electron > 0) box%cc(:, :, i_electron) = 0.0_dp
    if (i_1pos_ion  > 0) box%cc(:, :, i_1pos_ion) = 0.0_dp
  else
    box%cc(:, :, i_eps) = 1.0_dp
  end if
end subroutine my_init_cond


end module m_user
