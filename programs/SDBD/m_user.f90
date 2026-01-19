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

  ! Absolute positions in meters (NOT relative)
  real(dp) :: user_powered1_pos(NDIM)  = [0.0_dp, 0.0_dp]
  real(dp) :: user_powered2_pos(NDIM)  = [0.0_dp, 0.0_dp]
  real(dp) :: user_grounded1_pos(NDIM) = [0.0_dp, 0.0_dp]
  real(dp) :: user_grounded2_pos(NDIM) = [0.0_dp, 0.0_dp]

  real(dp) :: powered1(NDIM), powered2(NDIM)
  real(dp) :: grounded1(NDIM), grounded2(NDIM)

  ! Small potential offset for the "grounded" electrode (Volts)
  real(dp) :: grounded_phi_offset = 1.0e-3_dp

  ! Cut depth d (m) for circular segment electrodes:
  ! d = R   -> half-disk
  ! d = 2R  -> full disk
  ! d = 0   -> (almost) empty
  real(dp) :: user_cut_depth = -1.0_dp

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

    call CFG_add_get(cfg, "user_powered1_pos",  user_powered1_pos,  "Powered electrode 1 (m)")
    call CFG_add_get(cfg, "user_powered2_pos",  user_powered2_pos,  "Powered electrode 2 (m)")
    call CFG_add_get(cfg, "user_grounded1_pos", user_grounded1_pos, "Grounded electrode 1 (m)")
    call CFG_add_get(cfg, "user_grounded2_pos", user_grounded2_pos, "Grounded electrode 2 (m)")

    call CFG_add_get(cfg, "grounded_phi_offset", grounded_phi_offset, &
         "Small offset potential (V) applied to grounded electrode")

    call CFG_add_get(cfg, "user_cut_depth", user_cut_depth, &
         "Cut depth d (m) for circular segment; d=R half-disk, d=2R full disk")

    call CFG_add_get(cfg, "dielectric_eps", dielectric_eps, "Dielectric permittivity")
    call CFG_add_get(cfg, "slab_y0", slab_y0, "Dielectric slab lower y (m)")
    call CFG_add_get(cfg, "slab_y1", slab_y1, "Dielectric slab upper y (m)")

    ! Use absolute coordinates directly (no scaling by domain size)
    powered1  = user_powered1_pos
    powered2  = user_powered2_pos
    grounded1 = user_grounded1_pos
    grounded2 = user_grounded2_pos

    ! Default: half-disk if not specified
    if (user_cut_depth <= 0.0_dp) user_cut_depth = user_circle_radius

    user_lsf                => my_user_lsf
    user_lsf_bc             => my_user_lsf_bc
    user_initial_conditions => my_init_cond
  end subroutine user_initialize

  pure real(dp) function cutcircle_lsf(x, c, rad, d, from_top) result(lsf)
    ! Circular segment ("cap") of a disk:
    ! keep points inside circle AND inside a half-space defined by depth d.
    !
    ! d is measured from the extreme point along y:
    !   from_top = .true.  : keep y >= (c_y + rad - d)   (cut from top)
    !   from_top = .false. : keep y <= (c_y - rad + d)   (cut from bottom)
    !
    ! Special cases:
    !   d = rad    -> half disk
    !   d = 2*rad  -> full disk
    !   d = 0      -> (almost) empty (only tangent point)
    real(dp), intent(in) :: x(NDIM), c(NDIM)
    real(dp), intent(in) :: rad, d
    logical, intent(in)  :: from_top
    real(dp) :: d_circle, d_half, y_cut, dd

    dd = max(0.0_dp, min(d, 2.0_dp*rad))

    d_circle = norm2(x - c) - rad

    if (from_top) then
      y_cut  = c(2) + rad - dd
      d_half = y_cut - x(2)         ! <=0 when x(2) >= y_cut
    else
      y_cut  = c(2) - rad + dd
      d_half = x(2) - y_cut         ! <=0 when x(2) <= y_cut
    end if

    lsf = max(d_circle, d_half)     ! intersection of circle and half-space
  end function cutcircle_lsf

  real(dp) function my_user_lsf(x) result(lsf)
    real(dp), intent(in) :: x(NDIM)
    real(dp) :: lp, lg

    lp = min( cutcircle_lsf(x, powered1,  user_circle_radius, user_cut_depth, .true.), &
              cutcircle_lsf(x, powered2,  user_circle_radius, user_cut_depth, .true.) )

    lg = min( cutcircle_lsf(x, grounded1, user_circle_radius, user_cut_depth, .false.), &
              cutcircle_lsf(x, grounded2, user_circle_radius, user_cut_depth, .false.) )

    lsf = min(lp, lg)
  end function my_user_lsf

  real(dp) function my_user_lsf_bc(x) result(phi)
    real(dp), intent(in) :: x(NDIM)
    real(dp) :: mp, mg

    mp = min( cutcircle_lsf(x, powered1,  user_circle_radius, user_cut_depth, .true.), &
              cutcircle_lsf(x, powered2,  user_circle_radius, user_cut_depth, .true.) )

    mg = min( cutcircle_lsf(x, grounded1, user_circle_radius, user_cut_depth, .false.), &
              cutcircle_lsf(x, grounded2, user_circle_radius, user_cut_depth, .false.) )

    if (mp < mg) then
      phi = current_voltage
    else
      phi = grounded_phi_offset
    end if
  end function my_user_lsf_bc

  subroutine my_init_cond(box)
    type(box_t), intent(inout) :: box
    real(dp) :: y_center
    logical  :: in_slab

    if (.not. ST_use_dielectric) return
    if (i_eps <= 0) return

    y_center = box%r_min(2) + 0.5_dp * box%dr(2) * box%n_cell
    in_slab  = (y_center >= slab_y0 .and. y_center < slab_y1)

    if (in_slab) then
      box%cc(:, :, i_eps) = dielectric_eps
      if (i_electron > 0) box%cc(:, :, i_electron) = 0.0_dp
      if (i_1pos_ion  > 0) box%cc(:, :, i_1pos_ion) = 0.0_dp
    else
      box%cc(:, :, i_eps) = 1.0_dp
    end if
  end subroutine my_init_cond

end module m_user
