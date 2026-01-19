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

  ! Geometry parameters read from cfg
  real(dp) :: user_circle_radius = 1.0e-4_dp
  real(dp) :: user_powered1_rel(NDIM)  = [0.40_dp, 0.70_dp]
  real(dp) :: user_powered2_rel(NDIM)  = [0.60_dp, 0.70_dp]
  real(dp) :: user_grounded1_rel(NDIM) = [0.40_dp, 0.30_dp]
  real(dp) :: user_grounded2_rel(NDIM) = [0.60_dp, 0.30_dp]

  ! Absolute positions (computed after reading cfg)
  real(dp) :: powered1(NDIM), powered2(NDIM), grounded1(NDIM), grounded2(NDIM)

contains

  subroutine user_initialize(cfg, tree)
    type(CFG_t), intent(inout) :: cfg
    type(af_t),  intent(inout) :: tree

    call CFG_add_get(cfg, "user_circle_radius", user_circle_radius, &
         "Radius (m) of user-defined circle electrodes")

    call CFG_add_get(cfg, "user_powered1_rel", user_powered1_rel, &
         "Relative center (x,y) of powered circle 1")
    call CFG_add_get(cfg, "user_powered2_rel", user_powered2_rel, &
         "Relative center (x,y) of powered circle 2")
    call CFG_add_get(cfg, "user_grounded1_rel", user_grounded1_rel, &
         "Relative center (x,y) of grounded circle 1")
    call CFG_add_get(cfg, "user_grounded2_rel", user_grounded2_rel, &
         "Relative center (x,y) of grounded circle 2")

    ! Convert relative -> absolute coordinates
    powered1  = ST_domain_origin + user_powered1_rel  * ST_domain_len
    powered2  = ST_domain_origin + user_powered2_rel  * ST_domain_len
    grounded1 = ST_domain_origin + user_grounded1_rel * ST_domain_len
    grounded2 = ST_domain_origin + user_grounded2_rel * ST_domain_len

    ! Register electrode callbacks
    user_lsf    => my_user_lsf
    user_lsf_bc => my_user_lsf_bc
  end subroutine user_initialize

  ! Signed-distance-like function for a circle: ||x-c|| - radius
  pure real(dp) function circle_lsf(x, c, rad) result(lsf)
    real(dp), intent(in) :: x(NDIM), c(NDIM)
    real(dp), intent(in) :: rad
    lsf = norm2(x - c) - rad
  end function circle_lsf

  ! Union of 4 circles -> min of individual lsf values
  real(dp) function my_user_lsf(x) result(lsf)
    real(dp), intent(in) :: x(NDIM)
    real(dp) :: l1, l2, l3, l4

    l1 = circle_lsf(x, powered1,  user_circle_radius)
    l2 = circle_lsf(x, powered2,  user_circle_radius)
    l3 = circle_lsf(x, grounded1, user_circle_radius)
    l4 = circle_lsf(x, grounded2, user_circle_radius)

    lsf = min(min(l1, l2), min(l3, l4))
  end function my_user_lsf

  ! Boundary potential: powered circles -> current_voltage, grounded -> 0
  real(dp) function my_user_lsf_bc(x) result(phi)
    real(dp), intent(in) :: x(NDIM)
    real(dp) :: l1, l2, l3, l4
    real(dp) :: m_pow, m_gnd

    l1 = circle_lsf(x, powered1,  user_circle_radius)
    l2 = circle_lsf(x, powered2,  user_circle_radius)
    l3 = circle_lsf(x, grounded1, user_circle_radius)
    l4 = circle_lsf(x, grounded2, user_circle_radius)

    m_pow = min(l1, l2)
    m_gnd = min(l3, l4)

    if (m_pow < m_gnd) then
      phi = current_voltage
    else
      phi = 0.0_dp
    end if
  end function my_user_lsf_bc

end module m_user
