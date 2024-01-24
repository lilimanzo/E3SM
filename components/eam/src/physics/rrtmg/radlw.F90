
module radlw
!----------------------------------------------------------------------- 
! 
! Purpose: Longwave radiation calculations.
!
!-----------------------------------------------------------------------
use shr_kind_mod,      only: r8 => shr_kind_r8
use ppgrid,            only: pcols, pver, pverp
use scamMod,           only: single_column, scm_crm_mode
use parrrtm,           only: nbndlw, ngptlw
use rrtmg_lw_init,     only: rrtmg_lw_ini
use rrtmg_lw_rad,      only: rrtmg_lw
use spmd_utils,        only: masterproc
use perf_mod,          only: t_startf, t_stopf
use cam_logfile,       only: iulog
use cam_abortutils,        only: endrun
use radconstants,      only: nlwbands

implicit none

private
save

! Public methods

public ::&
   radlw_init,   &! initialize constants
   rad_rrtmg_lw   ! driver for longwave radiation code
   
! Private data
integer :: ntoplw    ! top level to solve for longwave cooling
logical :: pergro_mods = .false.
!===============================================================================
CONTAINS
!===============================================================================


subroutine radlw_init()
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize various constants for radiation scheme.
!
!-----------------------------------------------------------------------

   use ref_pres, only : pref_mid
   use phys_control, only: phys_getopts

   integer :: k
   
   call phys_getopts(pergro_mods_out=pergro_mods)

   ! If the top model level is above ~90 km (0.1 Pa), set the top level to compute
   ! longwave cooling to about 80 km (1 Pa)
   if (pref_mid(1) .lt. 0.1_r8) then
      do k = 1, pver
         if (pref_mid(k) .lt. 1._r8) ntoplw  = k
      end do
   else
      ntoplw  = 1
   end if
   if (masterproc) then
      write(iulog,*) 'radlw_init: ntoplw =',ntoplw
   endif

   call rrtmg_lw_ini

end subroutine radlw_init

!-------------------------------------------------------------------------------

end module radlw
