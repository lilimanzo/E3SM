#include "physics/mam/eamxx_mam_wetscav_process_interface.hpp"

#include <ekat/ekat_assert.hpp>

#include "scream_config.h"  // for SCREAM_CIME_BUILD

// Remove the following<<<<
#include <type_traits>
#include <typeinfo>
//>>>>>>>

/*
Future work:
Wirte comments
write in/outs for all variables clearly
*/

namespace scream {

// =========================================================================================
MAMWetscav::MAMWetscav(const ekat::Comm &comm,
                       const ekat::ParameterList &params)
    : AtmosphereProcess(comm, params) {
  /* Anything that can be initialized without grid information can be
   * initialized here. Like universal constants, mam wetscav options.
   */
}

AtmosphereProcessType MAMWetscav::type() const {
  return AtmosphereProcessType::Physics;
}

std::string MAMWetscav::name() const { return "mam4_wetscav"; }
// =========================================================================================
void MAMWetscav::set_grids(
    const std::shared_ptr<const GridsManager> grids_manager) {
  using namespace ekat::units;

  // The units of mixing ratio Q are technically non-dimensional.
  // Nevertheless, for output reasons, we like to see 'kg/kg'.
  auto q_unit    = kg / kg;
  auto dqdt_unit = kg / kg / s;
  auto n_unit    = 1 / kg;  // units of number mixing ratios of tracers

  m_grid                = grids_manager->get_grid("Physics");
  const auto &grid_name = m_grid->name();

  ncol_ = m_grid->get_num_local_dofs();       // Number of columns on this rank
  nlev_ = m_grid->get_num_vertical_levels();  // Number of levels per column
  const int nmodes    = mam4::AeroConfig::num_modes();  // Number of modes
  constexpr int pcnst = mam4::aero_model::pcnst;

  // layout for 3D (2d horiz X 1d vertical) variables at level
  // midpoints/interfaces
  FieldLayout scalar3d_mid = m_grid->get_3d_scalar_layout(true);
  FieldLayout scalar3d_int = m_grid->get_3d_scalar_layout(false);

  // layout for 2D (1d horiz X 1d vertical) variables
  FieldLayout scalar2d = m_grid->get_2d_scalar_layout();

  // layout for 3D (ncol, nmodes, nlevs)
  FieldLayout scalar3d_mid_nmodes =
      m_grid->get_3d_vector_layout(true, nmodes, "nmodes");

  // layout for 2D (ncol, pcnst)
  FieldLayout scalar2d_pconst = m_grid->get_2d_vector_layout(pcnst, "pcnst");
  // -------------------------------------------------------------------------------------------------------------------------
  // These variables are "required" or pure inputs for the process
  // -------------------------------------------------------------------------------------------------------------------------
  add_field<Required>("T_mid", scalar3d_mid, K,
                      grid_name);  // temperature [K]
  add_field<Required>("p_mid", scalar3d_mid, Pa,
                      grid_name);  // pressure at mid points in [Pa
  add_field<Required>("pseudo_density", scalar3d_mid, Pa,
                      grid_name);  // pseudo density in [Pa]
  add_field<Required>("qc", scalar3d_mid, q_unit, grid_name,
                      "tracers");  // liquid cloud water [kg/kg] wet
  add_field<Required>("qi", scalar3d_mid, q_unit, grid_name,
                      "tracers");  // ice cloud water [kg/kg] wet
  add_field<Required>("p_int", scalar3d_int, Pa,
                      grid_name);  // total pressure

  // -- Input variables that exists in PBUF in EAM
  static constexpr auto nondim =
      Units::nondimensional();  // for variables that are fractions etc.

  // Deep convective cloud fraction [fraction] //NOT updated
  add_field<Required>("dp_frac", scalar3d_mid, nondim, grid_name);

  // MUST FIXME: cldt and cldn are the same variables. They must be their
  // previous step values.
  add_field<Updated>("cldn", scalar3d_mid, nondim,
                     grid_name);  // layer cloud fraction [fraction]
  add_field<Updated>("cldt", scalar3d_mid, nondim,
                     grid_name);  //??
  add_field<Updated>("rprdsh", scalar3d_mid, kg / kg / s,
                     grid_name);  // rain production, shallow convection
                                  // [kg/kg/s] //NOT updated
  add_field<Updated>(
      "rprddp", scalar3d_mid, kg / kg / s,
      grid_name);  // rain production, deep convection [kg/kg/s] //NOT updated
  add_field<Updated>("evapcsh", scalar3d_mid, kg / kg / s,
                     grid_name);  // Evaporation rate of shallow convective
                                  // //NOT updated precipitation >=0. [kg/kg/s]
  add_field<Updated>("evapcdp", scalar3d_mid, kg / kg / s,
                     grid_name);  // Evaporation rate of deep convective //NOT
                                  // updated precipitation >=0. [kg/kg/s]

  // -- Input variables that exists in PBUF in EAM (in wetdep.F90) in the
  // "inputs" data structure
  // MUST FIXME: cldt and cldn are the same variables. They must be their
  // previous step values.
  // add_field<Updated>("cldt_prev_step", scalar3d_mid, nondim,
  //                     grid_name);  // total cloud fraction [fraction]

  // FIXME: we do not need qme
  // add_field<Required>(
  //     "qme", scalar3d_mid, kg / kg / s,
  //     grid_name);  // net condensation/evaporation of cloud water [kg/kg/s]
  add_field<Updated>("prain", scalar3d_mid, kg / kg / s,
                     grid_name);  // stratiform rain production rate [kg/kg/s]
  add_field<Updated>(
      "evapr", scalar3d_mid, kg / kg / s,
      grid_name);  // evaporation from stratiform rain [kg/kg/s] //NOT updated

  // -- Input variables that exists in PBUF in EAM (in wetdep.F90)
  add_field<Updated>("icwmrdp", scalar3d_mid, kg / kg,
                     grid_name);  // In cloud water mixing ratio, deep
                                  // convection [kg/kg] //NOT updated
  add_field<Updated>("icwmrsh", scalar3d_mid, kg / kg,
                     grid_name);  // In cloud water mixing ratio, shallow
                                  // convection [kg/kg] //NOT updated
  // add_field<Required>("rprddp", scalar3d_mid, kg / kg / s,
  //                     grid_name);  // Rain production, deep convection
  //                     [kg/kg/s]
  add_field<Updated>(
      "sh_frac", scalar3d_mid, nondim,
      grid_name);  // Shallow convective cloud fraction [fraction] //NOT updated

  // in cloud water mixing ratio, deep shallow [kg/kg]
  add_field<Updated>("icwmrsh", scalar3d_mid, nondim, grid_name);
  // add_field<Updated>(
  //     "icwmrdp", scalar3d_mid, nondim,
  //     grid_name);
  // add_field<Required>("evapcsh", scalar3d_mid, kg / kg / s,
  //                     grid_name);  // Evaporation rate of shallow convective
  //                                  // precipitation >=0. [kg/kg/s]
  // add_field<Required>("evapcdp", scalar3d_mid, kg / kg / s,
  //                     grid_name);  // Evaporation rate of deep convective
  //                                  // precipitation >=0. [kg/kg/s]

  // -------------------------------------------------------------------------------------------------------------------------
  // These variables are "updated" or inputs/outputs for the process
  // -------------------------------------------------------------------------------------------------------------------------
  // FIXME: we have not added code to update the surface fluxes.
  // -- surface fluxes (input/outpts) for the coupler's cam_out data struture
  // for the land model
  static constexpr auto m2 = m * m;
  add_field<Updated>(
      "bcphiwet", scalar3d_mid, kg / m2 / s,
      grid_name);  // wet deposition of hydrophilic black carbon [kg/m2/s]
  add_field<Updated>(
      "bcphidry", scalar3d_mid, kg / m2 / s,
      grid_name);  // dry deposition of hydrophilic black carbon [kg/m2/s]
  add_field<Updated>(
      "ocphiwet", scalar3d_mid, kg / m2 / s,
      grid_name);  // wet deposition of hydrophilic organic carbon [kg/m2/s]
  add_field<Updated>(
      "ocphidry", scalar3d_mid, kg / m2 / s,
      grid_name);  // dry deposition of hydrophilic organic carbon [kg/m2/s]

  add_field<Updated>("dstwet1", scalar3d_mid, kg / m2 / s,
                     grid_name);  // wet deposition of dust (bin1) [kg/m2/s]
  add_field<Updated>("dstwet2", scalar3d_mid, kg / m2 / s,
                     grid_name);  // wet deposition of dust (bin2) [kg/m2/s]
  add_field<Updated>("dstwet3", scalar3d_mid, kg / m2 / s,
                     grid_name);  // wet deposition of dust (bin3) [kg/m2/s]
  add_field<Updated>("dstwet4", scalar3d_mid, kg / m2 / s,
                     grid_name);  // wet deposition of dust (bin4) [kg/m2/s]

  // -- input/ouputs from PBUF for updating particle size and water uptake by
  // particles
  static constexpr auto m3 = m2 * m;
  add_field<Updated>("dgncur_a", scalar3d_mid_nmodes, m,
                     grid_name);  // aerosol dry particle diameter [m]
  add_field<Updated>("wetdens", scalar3d_mid_nmodes, kg / m3,
                     grid_name);  // wet aerosol density [kg/m3]
  add_field<Updated>("qaerwat", scalar3d_mid_nmodes, kg / kg,
                     grid_name);  // aerosol water [kg/kg]
  //
  add_field<Updated>("dgnumwet", scalar3d_mid_nmodes, m,
                     grid_name);  // wet aerosol diameter [m]
  add_field<Updated>("fracis", scalar3d_mid, nondim,
                     grid_name);  // fraction of transported species that are
                                  // insoluble [fraction]

  // -- interstitial and cloudborne aerosol tracers of interest: mass (q) and
  // number (n) mixing ratios
  // -- NOTE: Interstitial aerosols are updated in the interface using the
  // "tendencies" from the wetscavenging process
  for(int imode = 0; imode < mam_coupling::num_aero_modes(); ++imode) {
    // interstitial aerosol tracers of interest: number (n) mixing ratios
    const char *int_nmr_field_name =
        mam_coupling::int_aero_nmr_field_name(imode);
    add_field<Updated>(int_nmr_field_name, scalar3d_mid, n_unit, grid_name,
                       "tracers");

    // cloudborne aerosol tracers of interest: number (n) mixing ratios
    const char *cld_nmr_field_name =
        mam_coupling::cld_aero_nmr_field_name(imode);

    // NOTE: DO NOT add cld borne aerosols to the "tracer" group as these are
    // NOT advected
    add_field<Updated>(cld_nmr_field_name, scalar3d_mid, n_unit, grid_name);

    for(int ispec = 0; ispec < mam_coupling::num_aero_species(); ++ispec) {
      // (interstitial) aerosol tracers of interest: mass (q) mixing ratios
      const char *int_mmr_field_name =
          mam_coupling::int_aero_mmr_field_name(imode, ispec);
      if(strlen(int_mmr_field_name) > 0) {
        add_field<Updated>(int_mmr_field_name, scalar3d_mid, q_unit, grid_name,
                           "tracers");
      }

      // (cloudborne) aerosol tracers of interest: mass (q) mixing ratios
      const char *cld_mmr_field_name =
          mam_coupling::cld_aero_mmr_field_name(imode, ispec);
      if(strlen(cld_mmr_field_name) > 0) {
        // NOTE: DO NOT add cld borne aerosols to the "tracer" group as these
        // are NOT advected
        add_field<Updated>(cld_mmr_field_name, scalar3d_mid, q_unit, grid_name);
      }
    }
  }

  // Tracers group -- do we need this in addition to the tracers above? In any
  // case, this call should be idempotent, so it can't hurt.
  add_group<Updated>("tracers", grid_name, 1, Bundling::Required);

  // The following fields are not needed by this process but we define them so
  //  that we can create MAM4xx class objects like atmosphere, prognostics etc.

  // aerosol-related gases: mass mixing ratios
  for(int g = 0; g < mam_coupling::num_aero_gases(); ++g) {
    const char *gas_mmr_field_name = mam_coupling::gas_mmr_field_name(g);
    add_field<Updated>(gas_mmr_field_name, scalar3d_mid, q_unit, grid_name,
                       "tracers");
  }

  // aerosol-related gases: mass mixing ratios
  for(int g = 0; g < mam_coupling::num_aero_gases(); ++g) {
    std::string ptend_gas_name =
        "ptend_" + std::string(mam_coupling::gas_mmr_field_name(g));
    add_field<Computed>(ptend_gas_name, scalar3d_mid, dqdt_unit, grid_name);
  }

  // tendencies for interstitial aerosols
  for(int imode = 0; imode < mam_coupling::num_aero_modes(); ++imode) {
    std::string ptend_num =
        "ptend_" + std::string(mam_coupling::int_aero_nmr_field_name(imode));
    add_field<Computed>(ptend_num, scalar3d_mid, n_unit, grid_name);
    for(int ispec = 0; ispec < mam_coupling::num_aero_species(); ++ispec) {
      // (interstitial) aerosol tracers of interest: mass (q) mixing ratios
      const char *int_mmr_field_name =
          mam_coupling::int_aero_mmr_field_name(imode, ispec);
      if(strlen(int_mmr_field_name) > 0) {
        std::string ptend_int_mmr_field_name =
            "ptend_" + std::string(int_mmr_field_name);
        add_field<Computed>(ptend_int_mmr_field_name, scalar3d_mid, dqdt_unit,
                            grid_name);
      }
    }
  }

  add_field<Required>("cldfrac_tot", scalar3d_mid, nondim,
                      grid_name);  // Cloud fraction
  add_field<Required>("pbl_height", scalar2d, m,
                      grid_name);  // PBL height

  static constexpr auto s2 = s * s;
  add_field<Required>("phis", scalar2d, m2 / s2,
                      grid_name);  // surface geopotential
  add_field<Required>("qv", scalar3d_mid, q_unit,
                      grid_name);  // specific humidity
  add_field<Required>("nc", scalar3d_mid, n_unit,
                      grid_name);  // cloud water number conc
  add_field<Required>("ni", scalar3d_mid, n_unit,
                      grid_name);  // cloud ice number conc
  add_field<Required>("omega", scalar3d_mid, Pa / s,
                      grid_name);  // vertical pressure velocity

  // FIXME: units
  add_field<Updated>("dlf", scalar3d_mid, kg / kg / s,
                     grid_name);  //
  // add_field<Updated>("dp_ccf", scalar3d_mid, n_unit,
  //                     grid_name);  //
  // add_field<Updated>("sh_ccf", scalar3d_mid, n_unit,
  //                     grid_name);  //
  // aerosol wet deposition (interstitial)
  add_field<Computed>("aerdepwetis", scalar2d_pconst, kg / m2 / s,
                      grid_name);  //
  // aerosol wet deposition (cloud water)
  add_field<Computed>("aerdepwetcw", scalar2d_pconst, kg / m2 / s,
                      grid_name);  //
}

// =========================================================================================
// ON HOST, returns the number of bytes of device memory needed by the above
// Buffer type given the number of columns and vertical levels
size_t MAMWetscav::requested_buffer_size_in_bytes() const {
  return mam_coupling::buffer_size(ncol_, nlev_);
}

// =========================================================================================
// ON HOST, initializeѕ the Buffer type with sufficient memory to store
// intermediate (dry) quantities on the given number of columns with the given
// number of vertical levels. Returns the number of bytes allocated.
void MAMWetscav::init_buffers(const ATMBufferManager &buffer_manager) {
  EKAT_REQUIRE_MSG(
      buffer_manager.allocated_bytes() >= requested_buffer_size_in_bytes(),
      "Error! Insufficient buffer size.\n");

  size_t used_mem =
      mam_coupling::init_buffer(buffer_manager, ncol_, nlev_, buffer_);
  EKAT_REQUIRE_MSG(used_mem == requested_buffer_size_in_bytes(),
                   "Error! Used memory != requested memory for MAMWetscav.");
}

// =========================================================================================
void MAMWetscav::initialize_impl(const RunType run_type) {
  // Gather runtime options
  //(e.g.) runtime_options.lambda_low    = m_params.get<double>("lambda_low");

  // populate the wet atmosphere state with views from fields and
  // the buffer (NOTE: wet atmosphere only has qv, qc, qi, nc, ni and omega)
  wet_atm_.qc = get_field_in("qc").get_view<const Real **>();
  wet_atm_.qi = get_field_in("qi").get_view<const Real **>();

  // -- Following wet atm variables are NOT used by the process but we still
  // need them to
  // -- create atmosphere object
  wet_atm_.qv    = get_field_in("qv").get_view<const Real **>();
  wet_atm_.nc    = get_field_in("nc").get_view<const Real **>();
  wet_atm_.ni    = get_field_in("ni").get_view<const Real **>();
  wet_atm_.omega = get_field_in("omega").get_view<const Real **>();

  // populate the dry atmosphere state with views from fields
  // (NOTE: dry atmosphere has everything that wet
  // atmosphere has along with z_surf, T_mid, p_mid, z_mid, z_iface,
  // dz, p_del, cldfrac, w_updraft, pblh, phis)
  dry_atm_.T_mid = get_field_in("T_mid").get_view<const Real **>();
  dry_atm_.p_mid = get_field_in("p_mid").get_view<const Real **>();
  dry_atm_.p_del = get_field_in("pseudo_density").get_view<const Real **>();
  dry_atm_.p_int = get_field_in("p_int").get_view<const Real **>();
  // How "buffer_" works: We use buffer to allocate memory for the members of
  // dry_atm_ object. Here we are providing those memory locations to the
  // dry_atm_ members. These members are computed from the above wet_atm_ or
  // dry_atm_ members that are explicitly getting their values either from the
  // input file or from other processes. These members are null at this point,
  // they are assigned in "Kokkos::parallel_for("preprocess", scan_policy,
  // preprocess_);" call in the run_impl

  dry_atm_.qv        = buffer_.qv_dry;
  dry_atm_.qc        = buffer_.qc_dry;
  dry_atm_.nc        = buffer_.nc_dry;
  dry_atm_.qi        = buffer_.qi_dry;
  dry_atm_.ni        = buffer_.ni_dry;
  dry_atm_.z_mid     = buffer_.z_mid;
  dry_atm_.dz        = buffer_.dz;
  dry_atm_.z_iface   = buffer_.z_iface;
  dry_atm_.w_updraft = buffer_.w_updraft;

  // The following dry_atm_ members  *may* not be used by the process but they
  // are needed for creating MAM4xx class objects like Atmosphere
  dry_atm_.cldfrac = get_field_in("cldfrac_tot").get_view<const Real **>();
  dry_atm_.pblh    = get_field_in("pbl_height").get_view<const Real *>();
  dry_atm_.phis    = get_field_in("phis").get_view<const Real *>();
  dry_atm_.z_surf  = 0.0;
  // ---- set wet/dry aerosol-related gas state data
  for(int g = 0; g < mam_coupling::num_aero_gases(); ++g) {
    const char *mmr_field_name = mam_coupling::gas_mmr_field_name(g);
    wet_aero_.gas_mmr[g] = get_field_out(mmr_field_name).get_view<Real **>();
    dry_aero_.gas_mmr[g] = buffer_.dry_gas_mmr[g];
  }

  // set wet/dry aerosol state data (interstitial aerosols only)
  for(int imode = 0; imode < mam_coupling::num_aero_modes(); ++imode) {
    const char *int_nmr_field_name =
        mam_coupling::int_aero_nmr_field_name(imode);
    wet_aero_.int_aero_nmr[imode] =
        get_field_out(int_nmr_field_name).get_view<Real **>();
    dry_aero_.int_aero_nmr[imode] = buffer_.dry_int_aero_nmr[imode];

    const char *cld_nmr_field_name =
        mam_coupling::cld_aero_nmr_field_name(imode);
    wet_aero_.cld_aero_nmr[imode] =
        get_field_out(cld_nmr_field_name).get_view<Real **>();
    dry_aero_.cld_aero_nmr[imode] = wet_aero_.cld_aero_nmr[imode];

    for(int ispec = 0; ispec < mam_coupling::num_aero_species(); ++ispec) {
      const char *int_mmr_field_name =
          mam_coupling::int_aero_mmr_field_name(imode, ispec);
      if(strlen(int_mmr_field_name) > 0) {
        wet_aero_.int_aero_mmr[imode][ispec] =
            get_field_out(int_mmr_field_name).get_view<Real **>();
        dry_aero_.int_aero_mmr[imode][ispec] =
            buffer_.dry_int_aero_mmr[imode][ispec];
      }

      const char *cld_mmr_field_name =
          mam_coupling::cld_aero_mmr_field_name(imode, ispec);
      if(strlen(cld_mmr_field_name) > 0) {
        wet_aero_.cld_aero_mmr[imode][ispec] =
            get_field_out(cld_mmr_field_name).get_view<Real **>();
        dry_aero_.cld_aero_mmr[imode][ispec] =
            buffer_.dry_cld_aero_mmr[imode][ispec];
      }
    }
  }

  // ---- set aerosol-related gas tendencies  data
  for(int g = 0; g < mam_coupling::num_aero_gases(); ++g) {
    std::string ptend_mmr_field_name =
        "ptend_" + std::string(mam_coupling::gas_mmr_field_name(g));
    dry_aero_tends_.gas_mmr[g] =
        get_field_out(ptend_mmr_field_name).get_view<Real **>();
  }

  // set  aerosol state tendencies data (interstitial aerosols only)
  for(int imode = 0; imode < mam_coupling::num_aero_modes(); ++imode) {
    std::string ptend_int_nmr_field_name =
        "ptend_" + std::string(mam_coupling::int_aero_nmr_field_name(imode));
    dry_aero_tends_.int_aero_nmr[imode] =
        get_field_out(ptend_int_nmr_field_name).get_view<Real **>();

    for(int ispec = 0; ispec < mam_coupling::num_aero_species(); ++ispec) {
      const char *int_mmr_field_name =
          mam_coupling::int_aero_mmr_field_name(imode, ispec);
      if(strlen(int_mmr_field_name) > 0) {
        std::string ptend_int_aero_mmr_field_name =
            "ptend_" + std::string(int_mmr_field_name);
        dry_aero_tends_.int_aero_mmr[imode][ispec] =
            get_field_out(ptend_int_aero_mmr_field_name).get_view<Real **>();
      }
    }
  }

  // set up our preprocess/postprocess functors
  // Here we initialize (not compute) objects in preprocess struct using the
  // objects in the argument list
  preprocess_.initialize(ncol_, nlev_, wet_atm_, wet_aero_, dry_atm_,
                         dry_aero_);

  postprocess_.initialize(ncol_, nlev_, wet_atm_, wet_aero_, dry_atm_,
                          dry_aero_);

  // wetdep

  const int work_len = mam4::wetdep::get_aero_model_wetdep_work_len();
  work_              = view_2d("work", ncol_, work_len);
}

// =========================================================================================
void MAMWetscav::run_impl(const double dt) {
  const auto scan_policy = ekat::ExeSpaceUtils<
      KT::ExeSpace>::get_thread_range_parallel_scan_team_policy(ncol_, nlev_);
  // preprocess input -- needs a scan for the calculation of all variables
  // needed by this process or setting up MAM4xx classes and their objects
  Kokkos::parallel_for("preprocess", scan_policy, preprocess_);
  Kokkos::fence();
  const mam_coupling::DryAtmosphere &dry_atm = dry_atm_;
  const auto &dry_aero                       = dry_aero_;
  const auto &work                           = work_;
  const auto &dry_aero_tends                 = dry_aero_tends_;

  // inputs/outputs
  auto dlf  = get_field_out("dlf").get_view<Real **>();
  auto cldn = get_field_out("cldn").get_view<Real **>();

  // where is cldt_prev_step used?
  // auto cldt_prev_step = get_field_out("cldt_prev_step").get_view< Real **>();
  // //FIXME: Is it same as cldn_prev_step??
  auto cldt  = get_field_out("cldt").get_view<Real **>();  //??
  auto evapr = get_field_out("evapr").get_view<Real **>();
  auto rprdsh =
      get_field_out("rprdsh").get_view<Real **>();  // rain production, shallow

  // convection [kg/kg/s]
  auto evapcsh =
      get_field_out("evapcsh")
          .get_view<Real **>();  // Evaporation rate of shallow convective
                                 // precipitation >=0. [kg/kg/s]
  auto sh_frac =
      get_field_out("sh_frac")
          .get_view<Real **>();  // Shallow convective cloud fraction [fraction]
  auto rprddp =
      get_field_out("rprddp")
          .get_view<Real **>();  // rain production, deep convection [kg/kg/s]
  auto evapcdp =
      get_field_out("evapcdp")
          .get_view<Real **>();  //  Evaporation rate of deep convective
                                 //  precipitation >=0. [kg/kg/s]
  // Deep convective cloud fraction [fraction]
  auto dp_frac = get_field_in("dp_frac").get_view<const Real **>();

  auto icwmrsh = get_field_out("icwmrsh")
                     .get_view<Real **>();  // in cloud water mixing ratio,
                                            // shallow convection
  auto icwmrdp =
      get_field_out("icwmrdp")
          .get_view<Real **>();  // in cloud water mixing ratio, deep convection

  auto prain = get_field_out("prain")
                   .get_view<Real **>();  // stratiform rain production rate
  // outputs
  const auto aerdepwetis = get_field_out("aerdepwetis").get_view<Real **>();
  const auto aerdepwetcw = get_field_out("aerdepwetcw").get_view<Real **>();

  const auto wet_geometric_mean_diameter_i =
      get_field_out("dgnumwet").get_view<Real ***>();
  const auto dry_geometric_mean_diameter_i =
      get_field_out("dgncur_a").get_view<Real ***>();
  const auto qaerwat = get_field_out("qaerwat").get_view<Real ***>();
  const auto wetdens = get_field_out("wetdens").get_view<Real ***>();

  const auto policy =
      ekat::ExeSpaceUtils<KT::ExeSpace>::get_default_team_policy(ncol_, nlev_);

  // Making a local copy of 'nlev_' because we cannot use a member of a class
  // inside a parallel_for.
  const int nlev = nlev_;

  // loop over atmosphere columns and compute aerosol particle size
  Kokkos::parallel_for(
      policy, KOKKOS_LAMBDA(const ThreadTeam &team) {
        const int icol = team.league_rank();  // column index*/

        auto atm = mam_coupling::atmosphere_for_column(dry_atm, icol);
        // set surface state data
        // fetch column-specific subviews into aerosol prognostics
        mam4::Prognostics progs =
            mam_coupling::aerosols_for_column(dry_aero, icol);
        // fetch column-specific subviews into aerosol tendencies
        // Note: we are only updating interstitial aerosols.
        mam4::Tendencies tends =
            mam_coupling::interstitial_aerosols_tendencies_for_column(
                dry_aero_tends, icol);
        // shallow_convective_cloud_fraction
        auto cldn_icol = ekat::subview(cldn, icol);
        /// shallow_convective_precipitation_production
        auto rprdsh_icol = ekat::subview(rprdsh, icol);
        // deep_convective_precipitation_production
        auto rprddp_icol = ekat::subview(rprddp, icol);
        // deep_convective_precipitation_evaporation
        auto evapcdp_icol = ekat::subview(evapcdp, icol);
        // shallow_convective_precipitation_evaporation =
        auto evapcsh_icol = ekat::subview(evapcsh, icol);
        // deep_convective_cloud_fraction
        auto dp_frac_icol = ekat::subview(dp_frac, icol);
        // shallow_convective_cloud_fraction    =
        auto sh_frac_icol = ekat::subview(sh_frac, icol);
        // FIXME: what is this?

        auto icwmrdp_col  = ekat::subview(icwmrdp, icol);
        auto icwmrsh_icol = ekat::subview(icwmrsh, icol);
        auto evapr_icol   = ekat::subview(evapr, icol);
        auto cldt_icol    = ekat::subview(cldt, icol);

        auto dlf_icol         = ekat::subview(dlf, icol);
        auto aerdepwetis_icol = ekat::subview(aerdepwetis, icol);
        auto aerdepwetcw_icol = ekat::subview(aerdepwetcw, icol);
        auto work_icol        = ekat::subview(work, icol);
        auto wet_diameter_icol =
            ekat::subview(wet_geometric_mean_diameter_i, icol);
        auto dry_diameter_icol =
            ekat::subview(dry_geometric_mean_diameter_i, icol);
        auto qaerwat_icol = ekat::subview(qaerwat, icol);
        auto wetdens_icol = ekat::subview(wetdens, icol);
        auto prain_icol   = ekat::subview(prain, icol);

        mam4::wetdep::aero_model_wetdep(
            team, atm, progs, tends, dt,
            // inputs
            cldt_icol, cldn_icol, rprdsh_icol, rprddp_icol, evapcdp_icol,
            evapcsh_icol, dp_frac_icol, sh_frac_icol, icwmrdp_col, icwmrsh_icol,
            evapr_icol, dlf_icol, prain_icol,
            // in/out
            wet_diameter_icol, dry_diameter_icol, qaerwat_icol, wetdens_icol,
            // output
            aerdepwetis_icol, aerdepwetcw_icol, work_icol);
        team.team_barrier();
        // update interstitial aerosol state
        Kokkos::parallel_for(Kokkos::TeamVectorRange(team, nlev), [&](int kk) {
          for(int m = 0; m < mam_coupling::num_aero_modes(); ++m) {
            for(int a = 0; a < mam4::num_species_mode(m); ++a) {
              const auto q_aero_i       = progs.q_aero_i[m][a];
              const auto tends_q_aero_i = tends.q_aero_i[m][a];
              q_aero_i(kk) += tends_q_aero_i(kk) * dt;
            }
          }
        });
      });  // icol parallel_for loop
}

// =========================================================================================
}  // namespace scream
