



#ifndef CAM
#include "config.h"

module dcmip12_wrapper

! Implementation of the dcmip2012 dycore tests for the preqx dynamics target

use control_mod,          only: test_case, dcmip4_moist, dcmip4_X, vanalytic
use dcmip2012_test1_2_3,  only: test1_advection_deformation, test1_advection_hadley, test1_advection_orography, &
                                test2_steady_state_mountain, test2_schaer_mountain,test3_gravity_wave
use dcmip2012_test1_conv_mod, only: test1_conv_advection, test1_conv_print_results
use dcmip2012_test4,      only: test4_baroclinic_wave 
use mtests,               only: mtest_state
use derivative_mod,       only: derivative_t, gradient_sphere
use dimensions_mod,       only: np, nlev, nlevp, qsize, qsize_d, nelemd
use element_mod,          only: element_t
use element_state,        only: nt=>timelevels
use hybrid_mod,           only: hybrid_t
use hybvcoord_mod,        only: hvcoord_t, set_layer_locations
use kinds,                only: rl=>real_kind, iulog
use parallel_mod,         only: abortmp

! model specific routines - must be provided by each model:
use element_ops,          only: set_state, set_state_i, copy_state, tests_finalize, set_forcing_rayleigh_friction


implicit none

! physical constants used by dcmip2012 test 3.1
real(rl), parameter ::              &
  g       = 9.80616,                & ! grav const
  a       = 6371229.0,              & ! earth radius in meters
  Rd      = 287.0,                  & ! dry gas const
  cp      = 1004.5,                 & ! heat capacity const pressure
  kappa   = Rd/cp,                  &
  pi      = 3.141592654,            &
  p0      = 100000.0                  ! reference pressure

real(rl), dimension(:,:,:,:), allocatable :: u0, v0                     ! storage for dcmip2-x sponge layer
real(rl):: zi(nlevp), zm(nlev)                                          ! z coordinates
real(rl):: ddn_hyai(nlevp), ddn_hybi(nlevp)                             ! vertical derivativess of hybrid coefficients
real(rl):: tau
real(rl):: ztop
contains

!_____________________________________________________________________
subroutine dcmip2012_test1_1(elem,hybrid,hvcoord,nets,nete,time,n0,n1)

  ! 3d deformational flow

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat                                                    ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(4),dp,eta_dot,dp_dn       ! pointwise field values

  ! set analytic vertical coordinates at t=0
  if(.not. initialized) then
    if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 1-1: 3d deformational flow'
    call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                        ! get evenly spaced z levels
    hvcoord%etai  = exp(-zi/H)                                          ! set eta levels from z
    call set_hybrid_coefficients(hvcoord,hybrid, hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
    call set_layer_locations(hvcoord, .true., hybrid%masterthread)
    initialized = .true.
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H * log(1.0d0/hvcoord%etam(k))
      p = p0 * hvcoord%etam(k)
      call test1_advection_deformation(time,lon,lat,p,z,zcoords,u,v,w,T,phis,ps,rho,q(1),q(2),q(3),q(4))

      dp = pressure_thickness(ps,k,hvcoord)
      call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)
      if(time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))

  enddo; enddo; enddo; enddo

  ! set prescribed state at level interfaces
  do ie = nets,nete; do k=1,nlevp; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H  * log(1.0d0/hvcoord%etai(k))
      p = p0 * hvcoord%etai(k)
      call test1_advection_deformation(time,lon,lat,p,z,zcoords,u,v,w,T,phis,ps,rho,q(1),q(2),q(3),q(4))
      call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),n0,n1)

      ! get vertical derivative of p at point i,j,k
      dp_dn = ddn_hyai(k)*p0 + ddn_hybi(k)*ps

      ! get vertical eta velocity at point i,j,k
      eta_dot = -g*rho*w/p0

      ! store vertical mass flux
      elem(ie)%derived%eta_dot_dpdn_prescribed(i,j,k) = eta_dot * dp_dn

  enddo; enddo; enddo; enddo

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test1_2(elem,hybrid,hvcoord,nets,nete,time,n0,n1)

  !  Hadley-like Meridional Circulation

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat                                                    ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(2),dp,eta_dot,dp_dn       ! pointwise field values

  ! set analytic vertical coordinates at t=0
  if(.not. initialized) then
    if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 1-2: Hadley-like Meridional Circulation'
    call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                        ! get evenly spaced z levels
    hvcoord%etai  = exp(-zi/H)                                          ! set eta levels from z
    call set_hybrid_coefficients(hvcoord,hybrid, hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
    call set_layer_locations(hvcoord, .true., hybrid%masterthread)
    initialized = .true.
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H * log(1.0d0/hvcoord%etam(k))
      p = p0 * hvcoord%etam(k)
      call test1_advection_hadley(time,lon,lat,p,z,zcoords,u,v,w,t,phis,ps,rho,q(1),q(2))
      dp = pressure_thickness(ps,k,hvcoord)
      call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)
      if(time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))

  enddo; enddo; enddo; enddo

  ! set prescribed state at level interfaces
  do ie = nets,nete; do k=1,nlevp; do j=1,np; do i=1,np
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      z = H  * log(1.0d0/hvcoord%etai(k))
      p = p0 * hvcoord%etai(k)
      call test1_advection_hadley(time,lon,lat,p,z,zcoords,u,v,w,T,phis,ps,rho,q(1),q(2))
      call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),n0,n1)


      ! get vertical derivative of p at point i,j,k
      dp_dn = ddn_hyai(k)*p0 + ddn_hybi(k)*ps

      ! get vertical eta velocity at point i,j,k
      eta_dot = -g*rho*w/p0

      ! store vertical mass flux
      elem(ie)%derived%eta_dot_dpdn_prescribed(i,j,k) = eta_dot * dp_dn

  enddo; enddo; enddo; enddo

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test1_3(elem,hybrid,hvcoord,nets,nete,time,n0,n1,deriv)

  !  Horizontal advection of thin cloud-like tracers over orography

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type (derivative_t),intent(in)            :: deriv
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  integer,  parameter :: cfv     = 0                                    ! h-vel is not coordinate following
  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(4),dp,gc                 ! pointwise field values
  real(rl):: grad_p(np,np,2),p_i(np,np),u_i(np,np),v_i(np,np)

  ! set analytic vertical coordinates at t=0
  if(.not. initialized) then
    if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 1-3: Advection of thin clouds over orography'
    call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                        ! get evenly spaced z levels
    hvcoord%etai  = exp(-zi/H)                                          ! set eta levels from z
    call set_hybrid_coefficients(hvcoord,hybrid, hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
    call set_layer_locations(hvcoord, .true., hybrid%masterthread)
    initialized = .true.
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
      hyam=hvcoord%hyam(k); hybm=hvcoord%hybm(k)
      lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
      call test1_advection_orography(lon,lat,p,z,zcoords,cfv,use_eta,hyam,hybm,gc,u,v,w,t,phis,ps,rho,q(1),q(2),q(3),q(4))
      dp = pressure_thickness(ps,k,hvcoord)
      call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)
      if(time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))

  enddo; enddo; enddo; enddo

  ! set prescribed state at level interfaces
  do ie = nets,nete;
    do k=1,nlevp;
      do j=1,np; do i=1,np
        hyai=hvcoord%hyai(k); hybi=hvcoord%hybi(k)
        lon  = elem(ie)%spherep(i,j)%lon; lat  = elem(ie)%spherep(i,j)%lat
        call test1_advection_orography (lon,lat,p,z,zcoords,cfv,use_eta,hyai,hybi,gc,u,v,w,t,phis,ps,rho,q(1),q(2),q(3),q(4))
        call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),n0,n1)
        p_i(i,j) = p
        u_i(i,j) = u
        v_i(i,j) = v
      enddo; enddo

      ! get vertical mass flux
      grad_p = gradient_sphere(p_i,deriv,elem(ie)%Dinv)
      elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,k) = -u_i*grad_p(:,:,1) - v_i*grad_p(:,:,2)
    enddo;
    elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,1)     = 0
    elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,nlevp) = 0
  enddo;

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test1_conv(test_case,elem,hybrid,hvcoord,deriv,nets,nete,time,n0,n1)

  ! 3D tracer transport tests, modified to permit good convergence testing.

  ! Use physical constants consistent with HOMME
  use physical_constants, only: Rd => Rgas, p0

  character(len=*),   intent(in)            :: test_case
  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  type (derivative_t),intent(in)            :: deriv
  integer,            intent(in)            :: nets,nete                ! start, end element index
  real(rl),           intent(in)            :: time                     ! current time
  integer,            intent(in)            :: n0,n1                    ! time level indices

  logical ::  initialized = .false.

  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      ztop    = 12000.d0,     &                                         ! model top (m)
      H       = Rd * T0 / g                                             ! scale height

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyai,hyam,hybi,hybm                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(5),dp,eta_dot,dp_dn      ! pointwise field values
  logical :: use_w
  real(rl):: grad_p(np,np,2),p_i(np,np),u_i(np,np),v_i(np,np)

  ! set analytic vertical coordinates at t=0
  if (.not. initialized) then
     !$omp barrier
     !$omp master
     if (hybrid%masterthread) then
        write(iulog,*) 'initializing dcmip2012 test 3(a-e): &
             &modified 3d deformational flows for convergence testing'
     end if
     call get_evenly_spaced_z(zi,zm,0.0_rl,ztop)                        ! get evenly spaced z levels
     hvcoord%etai = exp(-zi/H)                                          ! set eta levels from z
     call set_hybrid_coefficients(hvcoord,hybrid,hvcoord%etai(1),1.0_rl)! set hybrid A and B from eta levels
     call set_layer_locations(hvcoord, .true., hybrid%masterthread)
     initialized = .true.
     !$omp end master
     !$omp barrier
  endif

  ! set prescribed state at level midpoints
  do ie = nets,nete; do k=1,nlev; do j=1,np; do i=1,np
     hyam = hvcoord%hyam(k); hybm = hvcoord%hybm(k)
     lon = elem(ie)%spherep(i,j)%lon; lat = elem(ie)%spherep(i,j)%lat
     z = H * log(1.0d0/hvcoord%etam(k))
     p = p0 * hvcoord%etam(k)
     call test1_conv_advection(test_case,time,lon,lat,hyam,hybm,p,z,u,v,w,use_w, &
          &                    T,phis,ps,rho,q)
     dp = pressure_thickness(ps,k,hvcoord)
     call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),n0,n1)
     if (time==0) call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
  enddo; enddo; enddo; enddo

  ! set prescribed state at level interfaces
  do ie = nets,nete
     do k = 1,nlevp
        do j = 1,np
           do i = 1,np
              hyai = hvcoord%hyai(k); hybi = hvcoord%hybi(k)
              lon = elem(ie)%spherep(i,j)%lon; lat = elem(ie)%spherep(i,j)%lat
              z = H  * log(1.0d0/hvcoord%etai(k))
              p = p0 * hvcoord%etai(k)
              call test1_conv_advection(test_case,time,lon,lat,hyai,hybi,p,z,u,v,w,use_w, &
                   &                    T,phis,ps,rho,q)
              call set_state_i(u,v,w,T,ps,phis,p,zi(k),g,i,j,k,elem(ie),n0,n1)
              if (use_w) then
                 ! get vertical derivative of p at point i,j,k
                 dp_dn = ddn_hyai(k)*p0 + ddn_hybi(k)*ps
                 ! get vertical eta velocity at point i,j,k
                 eta_dot = -g*rho*w/p0
                 ! store vertical mass flux
                 elem(ie)%derived%eta_dot_dpdn_prescribed(i,j,k) = eta_dot * dp_dn
              else
                 p_i(i,j) = p
                 u_i(i,j) = u
                 v_i(i,j) = v
              end if
           enddo
        enddo
        if (.not. use_w) then
           ! get vertical mass flux
           grad_p = gradient_sphere(p_i,deriv,elem(ie)%Dinv)
           elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,k) = -u_i*grad_p(:,:,1) - v_i*grad_p(:,:,2)
        end if
     enddo
     if (.not. use_w) then
        elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,1)     = 0
        elem(ie)%derived%eta_dot_dpdn_prescribed(:,:,nlevp) = 0
     end if
  enddo
end subroutine dcmip2012_test1_conv

!_____________________________________________________________________
subroutine dcmip2012_test2_0(elem,hybrid,hvcoord,nets,nete)

  ! steady state atmosphere with orography

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::      &
      T0      = 300.d0,       &                                         ! temperature (K)
      gamma   = 0.0065d0,     &                                         ! temperature lapse rate (K/m)
      ztop    = 12000.d0,     &                                         ! model top (m)
      exponent= g/(Rd*gamma)

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(1),dp    ! pointwise field values
  real(rl):: dpp(np,np,nlev), he

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 2-0: steady state atmosphere with orography'

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
  hvcoord%etai  = (1.d0 - gamma/T0*zi)**exponent                        ! set eta levels from z in orography-free region
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete; 
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call test2_steady_state_mountain(lon,lat,p,z,zcoords,use_eta,hyam,hybm,u,v,w,T,phis,ps,rho,q(1))
        dp = pressure_thickness(ps,k,hvcoord)
        !let's get an analytical \phi
        he = (T0 - T)/gamma
        call set_state(u,v,w,T,ps,phis,p,dp,he,g, i,j,k,elem(ie),1,nt)
        call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call test2_steady_state_mountain(lon,lat,p,z,zcoords,use_eta,hyai,hybi,u,v,w,T,phis,ps,rho,q(1))
        !let's get an analytical \phi
        he = (T0 - T)/gamma
        call set_state_i(u,v,w,T,ps,phis,p,he,g, i,j,k,elem(ie),1,nt)
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo
  
  end subroutine dcmip2012_test2_0



!_____________________________________________________________________
subroutine dcmip2012_test2_x(elem,hybrid,hvcoord,nets,nete,shear)

  ! nonhydrostatic orographic waves (with or without shear)

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  integer,            intent(in)            :: shear                    ! flag: 1=shear 0=no shear

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords
  real(rl), parameter ::   &
      Teq     = 300.d0,    &                                            ! temperature at equator
      ztop    = 30000.d0,	 &                                            ! model top (m)
      H       = Rd*Teq/g                                                ! characteristic height scale

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(1),dp    ! pointwise field values

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 2-x: steady state atmosphere with orography'

  !set \tau to 25.0, bound to X, [\tau]=[sec]
  tau = 25.0d0

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
  hvcoord%etai  = exp(-zi/H)                                            ! set eta levels from z in orography-free region
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete; 
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call test2_schaer_mountain(lon,lat,p,z,zcoords,use_eta,hyam,hybm,shear,u,v,w,T,phis,ps,rho,q(1))
        dp = pressure_thickness(ps,k,hvcoord)
        ! original
        !    call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),1,nt)
        ! This test obtains analytical height and returns it, so, we use it for \phi ...
        call set_state(u,v,w,T,ps,phis,p,dp,z,g, i,j,k,elem(ie),1,nt)
        call set_tracers(q,qsize,dp,i,j,k,lat,lon,elem(ie))
        ! ... or we can use discrete hydro state to init \phi. 
        
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call test2_schaer_mountain(lon,lat,p,z,zcoords,use_eta,hyai,hybi,shear,u,v,w,T,phis,ps,rho,q(1))
        call set_state_i(u,v,w,T,ps,phis,p,z,g, i,j,k,elem(ie),1,nt)
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo

  ! store initial velocity fields for use in sponge layer
  allocate( u0(np,np,nlev,nelemd) )
  allocate( v0(np,np,nlev,nelemd) )

  do ie = nets,nete
    u0(:,:,:,ie) = elem(ie)%state%v(:,:,1,:,1)
    v0(:,:,:,ie) = elem(ie)%state%v(:,:,2,:,1)
  enddo

end subroutine


!_____________________________________________________________________
subroutine mtest_init(elem,hybrid,hvcoord,nets,nete,testid)

  ! nonhydrostatic orographic waves (with or without shear)

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  integer,            intent(in)            :: testid                   ! 1 is m1,2 is m2, 3 is m3

  real(rl), parameter ::   &
      Teq     = 300.d0,    &                                            ! temperature at equator
      H       = Rd*Teq/g                                                ! characteristic height scale
  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,phis_ps,ps,rho,q(1),dp    ! pointwise field values
  real(rl):: ztop

  if (testid .eq. 1) then
     ztop = 20000.d0
  else
     ztop = 30000.d0
  endif

  if (hybrid%masterthread) write(iulog,*) 'initializing m test'

  !set \tau to 25*3 for this test, [\tau]=[sec], X=500/3
  tau = 75.0d0

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
  hvcoord%etai  = exp(-zi/H)                                            ! set eta levels from z in orography-free region
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete; 
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call mtest_state(lon,lat,p,z,hyam,hybm,u,v,w,T,phis,ps,rho,testid)
        dp = pressure_thickness(ps,k,hvcoord)
        call set_state(u,v,w,T,ps,phis,p,dp,z,g, i,j,k,elem(ie),1,nt)
        
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call mtest_state(lon,lat,p,z,hyai,hybi,u,v,w,T,phis,ps,rho,testid)
        call set_state_i(u,v,w,T,ps,phis,p,z,g, i,j,k,elem(ie),1,nt)
        
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo

  ! store initial velocity fields for use in sponge layer
  allocate( u0(np,np,nlev,nelemd) )
  allocate( v0(np,np,nlev,nelemd) )

  do ie = nets,nete
    u0(:,:,:,ie) = elem(ie)%state%v(:,:,1,:,1)
    v0(:,:,:,ie) = elem(ie)%state%v(:,:,2,:,1)
  enddo

end subroutine mtest_init


!_____________________________________________________________________
subroutine dcmip2012_test2_x_forcing(elem,hybrid,hvcoord,nets,nete,n,dt)

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(in)            :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index
  integer,            intent(in)            :: n                        ! time level index
  real(rl),           intent(in)            :: dt                       ! time-step size

  integer  :: ie, k
  real(rl) :: ztop, zc, z(np,np,nlev), z_i(np,np,nlevp)
  real(rl) :: f_d(nlev) 

  if (test_case == "mtest1") then
    ztop    = 20000.d0        ! model top
    zc      = 10000.d0        ! sponge-layer cutoff height
  else 
    ztop    = 30000.d0        ! model top
    zc      = 20000.d0        ! sponge-layer cutoff height
  endif

  forall(k=1:nlev) z(:,:,k)=zm(k)
  forall(k=1:nlevp) z_i(:,:,k)=zi(k)

  ! Compute damping as a function of layer-midpoint height
  !where(zm .ge. zh)
  !  f_d = sin(pi/2 *(zm - zh)/(ztop - zh))**2
  !elsewhere
  !  f_d = 0.0d0
  !end where

  ! apply sponge layer forcing to momentum terms
  !f_d = -f_d/tau

  do ie=nets,nete
     call set_forcing_rayleigh_friction(elem(ie),z,z_i,ztop,zc,tau,u0(:,:,:,ie),v0(:,:,:,ie),n)
  enddo


end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test3(elem,hybrid,hvcoord,nets,nete)

  ! nonhydrostatic gravity waves

  type(element_t),    intent(inout), target :: elem(:)                  ! element array
  type(hybrid_t),     intent(in)            :: hybrid                   ! hybrid parallel structure
  type(hvcoord_t),    intent(inout)         :: hvcoord                  ! hybrid vertical coordinates
  integer,            intent(in)            :: nets,nete                ! start, end element index

  integer,  parameter :: zcoords = 0                                    ! we are not using z coords
  logical,  parameter :: use_eta = .true.                               ! we are using hybrid eta coords

  real(rl), parameter ::    &                                           ! parameters needed to get eta from z
    T0      = 300.d0,       &	! temperature (k)
    ztop    = 10000.d0,     & ! model top (m)
    N       = 0.01d0,       & ! Brunt-Vaisala frequency
    bigG    = (g*g)/(N*N*Cp)  ! temperature, isothermal

  integer :: i,j,k,ie                                                   ! loop indices
  real(rl):: lon,lat,hyam,hybm,hyai,hybi                                ! pointwise coordiantes
  real(rl):: p,z,phis,u,v,w,T,T_mean,phis_ps,ps,rho,rho_mean,q(1),dp    ! pointwise field values

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 3-0: nonhydrostatic gravity waves'

  ! set analytic vertical coordinates
  call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                   ! get evenly spaced z levels
  hvcoord%etai  = ( (bigG/T0)*(exp(-zi*N*N/g) -1 )+1 ) **(1.0/kappa)    ! set eta levels from z at equator
  call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
  call set_layer_locations(hvcoord, .true., hybrid%masterthread)

  ! set initial conditions
  do ie = nets,nete
     do k=1,nlev; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)
        call test3_gravity_wave(lon,lat,p,z,zcoords,use_eta,hyam,hybm,u,v,w,T,T_mean,phis,ps,rho,rho_mean,q(1))
        dp = pressure_thickness(ps,k,hvcoord)
        call set_state(u,v,w,T,ps,phis,p,dp,zm(k),g, i,j,k,elem(ie),1,nt)
        call set_tracers(q,qsize, dp,i,j,k,lat,lon,elem(ie))
     enddo; enddo; enddo; 
     do k=1,nlevp; do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)
        call test3_gravity_wave(lon,lat,p,z,zcoords,use_eta,hyai,hybi,u,v,w,T,T_mean,phis,ps,rho,rho_mean,q(1))
        call set_state_i(u,v,w,T,ps,phis,p,zi(k),g, i,j,k,elem(ie),1,nt)
     enddo; enddo; enddo; 
     call tests_finalize(elem(ie),hvcoord)
  enddo

end subroutine

!_____________________________________________________________________
subroutine dcmip2012_test4_init(elem,hybrid,hvcoord,nets,nete)

  type(element_t),    intent(inout), target :: elem(:)
  type(hybrid_t),     intent(in)            :: hybrid 
  type(hvcoord_t),    intent(inout)         :: hvcoord        
  integer,            intent(in)            :: nets,nete      
  integer,  parameter :: zcoords = 0                          ! we are not using z coords
  logical,  parameter :: use_eta = .true.                     ! we are using hybrid eta coords

  real(rl), parameter :: ps_test = 100000.0d0

  integer :: i,j,k,ie                                                   !
  real(rl):: lon,lat,hyam,hybm,hyai,hybi
  real(rl):: p,z,phis,u,v,w,T,ps,rho,dp    !pointwise field values
  real(rl):: q,q1,q2,qarray(3),pressure
  integer :: qs
  real(rl):: ztop    = 10000.d0
  real(rl):: H       = Rd * 300d0 / g

  if (hybrid%masterthread) write(iulog,*) 'initializing dcmip2012 test 4: baroclinic wave'

  if (vanalytic==1) then
     if (hybrid%masterthread) write(iulog,*) 'using analytic veritcal coordinates'
     call get_evenly_spaced_z(zi,zm, 0.0_rl,ztop)                                    ! get evenly spaced z levels
     hvcoord%etai  = exp(-zi/H)                                            ! set eta levels from z in orography-free region
     call set_hybrid_coefficients(hvcoord,hybrid,  hvcoord%etai(1), 1.0_rl)! set hybrid A and B from eta levels
     call set_layer_locations(hvcoord, .true., hybrid%masterthread)
  endif
     
  ! set initial conditions

  do ie = nets,nete; 
    do k=1,nlev
      pressure=hvcoord%hyam(k)*hvcoord%ps0 + hvcoord%hybm(k)*ps_test
      do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyam,hybm, i,j,k,elem(ie),hvcoord)

        !test4_baroclinic_wave(moist,X,lon,lat,p,z,zcoords,u,v,w,t,phis,ps,rho,q,q1,q2)
        !moist 0 or 1, X is Earth scale factor, zcoord=0, q is vapor, q1, q2
        call test4_baroclinic_wave(dcmip4_moist,dcmip4_X,lon,lat,&
                                   pressure,z,zcoords,u,v,w,T,phis,ps,rho,q,q1,q2)
        qarray(1)=q; qarray(2)=q1; qarray(3)=q2;
        dp = pressure_thickness(ps,k,hvcoord)
        call set_state(u,v,w,T,ps,phis,pressure,dp,z,g, i,j,k,elem(ie),1,nt)

        !init only <=qsize tracers
        qs = min(qsize,3)
        call set_tracers(qarray(1:qs),qs, dp,i,j,k,lat,lon,elem(ie))
      enddo; enddo; enddo; 

    do k=1,nlevp
      pressure=hvcoord%hyai(k)*hvcoord%ps0 + hvcoord%hybi(k)*ps_test
      do j=1,np; do i=1,np
        call get_coordinates(lat,lon,hyai,hybi, i,j,k,elem(ie),hvcoord)

        !test4_baroclinic_wave(moist,X,lon,lat,p,z,zcoords,u,v,w,t,phis,ps,rho,q,q1,q2)
        !moist 0 or 1, X is Earth scale factor, zcoord=0, q is vapor, q1, q2
        call test4_baroclinic_wave(dcmip4_moist,dcmip4_X,lon,lat,&
                                   pressure,z,zcoords,u,v,w,T,phis,ps,rho,q,q1,q2)
        qarray(1)=q; qarray(2)=q1; qarray(3)=q2;
        call set_state_i(u,v,w,T,ps,phis,pressure,z,g, i,j,k,elem(ie),1,nt)

      enddo; enddo; enddo; 


    call tests_finalize(elem(ie),hvcoord)
  enddo ! ie loop
end subroutine dcmip2012_test4_init

!_____________________________________________________________________
subroutine get_evenly_spaced_z(zi,zm, zb,zt)

  real(rl), intent(in)    :: zb,zt      ! top and bottom coordinates
  real(rl), intent(inout) :: zi(nlevp)  ! z at interfaces
  real(rl), intent(inout) :: zm(nlev)   ! z at midpoints
  integer :: k

  forall(k=1:nlevp) zi(k) = zt-(k-1)*(zt-zb)/(nlevp-1)
  zm = 0.5_rl*( zi(2:nlevp) + zi(1:nlev) )

end subroutine

!_____________________________________________________________________
subroutine get_evenly_spaced_p(zi,zm,zb,zt,H)
  real(rl), intent(in)    :: zb,zt,H    ! top and bottom coordinates
  real(rl), intent(inout) :: zi(nlevp)  ! z at interfaces
  real(rl), intent(inout) :: zm(nlev)   ! z at midpoints
  integer :: k
  real(rl) :: etab, etat, deta

  etab = 1.0d0
  etat = exp(-zt/H)
  deta = (etab - etat)/nlev
  do k = 1, nlevp
     zi(k) = H*log(1.0d0/(etat + (k-1)*deta))
  end do
  zm = 0.5_rl*(zi(2:nlevp) + zi(1:nlev))
end subroutine get_evenly_spaced_p

!_____________________________________________________________________
subroutine set_hybrid_coefficients(hv, hybrid, eta_t, c)

  ! create an analytical set of A,B coefficients, given known eta levels

  type(hvcoord_t),    intent(inout) :: hv       ! hybrid vertical coordinate stucture
  type(hybrid_t),     intent(in)    :: hybrid   ! hybrid parallal structure
  real(rl),           intent(in)    :: eta_t    ! top eta level
  real(rl),           intent(in)    :: c        ! exponent

  real(rl)  :: eta_c, tmp
  integer   :: k

  ! place cutoff halfway between bottom and top eta coordiantes
  eta_c = hv%etai(nlev/2)

  ! place cutoff at model top
  eta_c = eta_t

  do k=1,nlevp
    ! get values of hybrid coefficients
    tmp        = max( (hv%etai(k)-eta_c)/(1.0-eta_c), 0.0_rl)
    hv%hybi(k) = tmp**c
    hv%hyai(k) = hv%etai(k) - hv%hybi(k)
    if(hybrid%masterthread) write(*,'(i4,a,f18.15,a,f18.15,a,f18.15)') &
         k,': etai=',hv%etai(k),' Ai=',hv%hyai(k),' Bi=',hv%hybi(k);

    ! get derivatives of hybrid coefficients
    ddn_hybi(k) = c*tmp**(c-1)
    if(hv%etai(k)>eta_c) ddn_hybi(k)=0.0d0
    ddn_hyai(k) = 1.0d0 - ddn_hybi(k)
  enddo

  hv%hyam = 0.5_rl *(hv%hyai(2:nlev+1) + hv%hyai(1:nlev))
  hv%hybm = 0.5_rl *(hv%hybi(2:nlev+1) + hv%hybi(1:nlev))
  hv%etam = hv%hyam + hv%hybm

end subroutine

!_____________________________________________________________________
subroutine get_coordinates(lat,lon,hyam,hybm, i,j,k,elem,hvcoord)

  ! get lat,lon, vertical coords at node(i,j,k)

  real(rl),         intent(out):: lon,lat,hyam,hybm
  integer,          intent(in) :: i,j,k
  type(element_t),  intent(in) :: elem
  type(hvcoord_t),  intent(in) :: hvcoord

  ! get horizontal coordinates at column i,j
  lon  = elem%spherep(i,j)%lon
  lat  = elem%spherep(i,j)%lat

  ! get hybrid coeffiecients at midpoint of vertical level k
  hyam = hvcoord%hyam(k)
  hybm = hvcoord%hybm(k)

end subroutine

!_____________________________________________________________________
real(rl) function pressure_thickness(ps,k,hv)

  real(rl),         intent(in) :: ps
  integer,          intent(in) :: k
  type(hvcoord_t),  intent(in) :: hv
  pressure_thickness = (hv%hyai(k+1)-hv%hyai(k))*p0 + (hv%hybi(k+1)-hv%hybi(k))*ps

end function


!_____________________________________________________________________
subroutine set_tracers(q,nq, dp,i,j,k,lat,lon,elem)

  ! set tracer values at node(i,j,k)

  real(rl),         intent(in)    :: q(nq), dp, lat, lon
  integer,          intent(in)    :: i,j,k,nq
  type(element_t),  intent(inout) :: elem

  real(rl), parameter :: wl = 1.0 ! checkerboard wavelength in dg
  integer :: qi

  if (nq>qsize) call abortmp('qsize set too small for dcmip test case')
  ! set known tracers to q and the rest to a checkerboard pattern
  elem%state%Q(i,j,k,1:nq) = q

  ! compute tracer mass qdp from mixing ratio q
  do qi = 1,nq
    elem%state%Qdp (i,j,k,qi,:) = q(qi)*dp
  enddo

  ! set any remaining tracers to 1
  do qi = nq+1,qsize
     elem%state%Q(i,j,k,qi)    = 1
     elem%state%Qdp (i,j,k,qi,:) = elem%state%Q(i,j,k,qi)*dp
  enddo


end subroutine

subroutine dcmip2012_print_test1_conv_results(test_case, elem, tl, hvcoord, par, subnum)
  use time_mod, only: timelevel_t
  use parallel_mod, only: parallel_t

  character(len=*), intent(in) :: test_case
  type(element_t), intent(in) :: elem(:)
  type(timelevel_t), intent(in) :: tl
  type(hvcoord_t), intent(in) :: hvcoord
  type(parallel_t), intent(in) :: par
  integer, intent(in) :: subnum

  call test1_conv_print_results(test_case, elem, tl, hvcoord, par, subnum)
end subroutine dcmip2012_print_test1_conv_results

end module dcmip12_wrapper
#endif
