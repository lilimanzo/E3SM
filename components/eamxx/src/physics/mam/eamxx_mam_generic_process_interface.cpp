#include <physics/mam/eamxx_mam_generic_process_interface.hpp>

namespace scream {
// ================================================================
//  Constructor
// ================================================================

MAMGenericInterface::MAMGenericInterface(const ekat::Comm &comm,
                                         const ekat::ParameterList &params)
    : AtmosphereProcess(comm, params) {
  /* Anything that can be initialized without grid information can be
   * initialized here. Like universal constants, mam wetscav options.
   */
}
// ================================================================
void MAMGenericInterface::get_aerosol_gas_map() {
  // NOTE: Using only one range for all num variables.
  // std::map<std::string, std::pair<Real, Real>> limits_aerosol_gas_tracers_;

  const std::string nmr_label = "nmr";
  const std::string mmr_label = "mmr";

  for(int mode = 0; mode < mam_coupling::num_aero_modes(); ++mode) {
    const std::string int_nmr_field_name =
        mam_coupling::int_aero_nmr_field_name(mode);
    limits_aerosol_gas_tracers_[int_nmr_field_name] =
        mam_coupling::physical_min_max(nmr_label);

    const std::string cld_nmr_field_name =
        mam_coupling::cld_aero_nmr_field_name(mode);
    limits_aerosol_gas_tracers_[cld_nmr_field_name] =
        mam_coupling::physical_min_max(nmr_label);

    for(int a = 0; a < mam_coupling::num_aero_species(); ++a) {
      const std::string int_mmr_field_name =
          mam_coupling::int_aero_mmr_field_name(mode, a);
      if(not int_mmr_field_name.empty()) {
        limits_aerosol_gas_tracers_[int_mmr_field_name] =
            mam_coupling::physical_min_max(mmr_label);
      }
      const std::string cld_mmr_field_name =
          mam_coupling::cld_aero_mmr_field_name(mode, a);
      if(not cld_mmr_field_name.empty()) {
        limits_aerosol_gas_tracers_[cld_mmr_field_name] =
            mam_coupling::physical_min_max(mmr_label);
      }
    }  // end for loop num species
  }

  for(int g = 0; g < mam_coupling::num_aero_gases(); ++g) {
    const std::string gas_mmr_field_name = mam_coupling::gas_mmr_field_name(g);
    limits_aerosol_gas_tracers_[gas_mmr_field_name] =
        mam_coupling::physical_min_max(mmr_label);
  }  // end for loop num gases
}

const std::pair<Real, Real> MAMGenericInterface::get_range(
    const std::string &field_name) {
  get_aerosol_gas_map();
  std::pair<Real, Real> min_max;

  auto it = limits_aerosol_gas_tracers_.find(field_name);
  if(it != limits_aerosol_gas_tracers_.end()) {
    min_max = it->second;
  } else {
    min_max = mam_coupling::physical_min_max(field_name);
    // std::cout << "Key not found" << std::endl;
  }
  return min_max;
}

// ================================================================
void MAMGenericInterface::add_tracers_cloudborne_aerosol() {
  using namespace ekat::units;
  auto q_unit           = kg / kg;  // units of mass mixing ratios of tracers
  auto n_unit           = 1 / kg;   // units of number mixing ratios of tracers
  const auto &grid_name = grid_->name();

  FieldLayout scalar3d_mid = grid_->get_3d_scalar_layout(true);

  // ---------------------------------------------------------------------
  // These variables are "Updated" or inputs/outputs for the process
  // ---------------------------------------------------------------------
  // NOTE: Cloud borne aerosols are not updated in this process but are included
  // to create data structures.

  // interstitial and cloudborne aerosol tracers of interest: mass (q) and
  // number (n) mixing ratios
  for(int mode = 0; mode < mam_coupling::num_aero_modes(); ++mode) {
    // cloudborne aerosol tracers of interest: number (n) mixing ratios
    // NOTE: DO NOT add cld borne aerosols to the "tracer" group as these are
    // NOT advected
    const std::string cld_nmr_field_name =
        mam_coupling::cld_aero_nmr_field_name(mode);
    add_field<Updated>(cld_nmr_field_name, scalar3d_mid, n_unit, grid_name);

    for(int a = 0; a < mam_coupling::num_aero_species(); ++a) {
      // (cloudborne) aerosol tracers of interest: mass (q) mixing ratios
      // NOTE: DO NOT add cld borne aerosols to the "tracer" group as these are
      // NOT advected
      const std::string cld_mmr_field_name =
          mam_coupling::cld_aero_mmr_field_name(mode, a);
      if(not cld_mmr_field_name.empty()) {
        add_field<Updated>(cld_mmr_field_name, scalar3d_mid, q_unit, grid_name);
      }
    }  // end for loop num species
  }    // end for loop for num modes
}

void MAMGenericInterface::add_tracers_interstitial_aerosol_and_gases() {
  using namespace ekat::units;
  auto q_unit           = kg / kg;  // units of mass mixing ratios of tracers
  auto n_unit           = 1 / kg;   // units of number mixing ratios of tracers
  const auto &grid_name = grid_->name();

  FieldLayout scalar3d_mid = grid_->get_3d_scalar_layout(true);

  // ---------------------------------------------------------------------
  // These variables are "Updated" or inputs/outputs for the process
  // ---------------------------------------------------------------------
  // NOTE: Cloud borne aerosols are not updated in this process but are included
  // to create data structures.

  // interstitial and cloudborne aerosol tracers of interest: mass (q) and
  // number (n) mixing ratios
  for(int mode = 0; mode < mam_coupling::num_aero_modes(); ++mode) {
    // interstitial aerosol tracers of interest: number (n) mixing ratios
    const std::string int_nmr_field_name =
        mam_coupling::int_aero_nmr_field_name(mode);
    add_tracer<Updated>(int_nmr_field_name, grid_, n_unit);
    for(int a = 0; a < mam_coupling::num_aero_species(); ++a) {
      // (interstitial) aerosol tracers of interest: mass (q) mixing ratios
      const std::string int_mmr_field_name =
          mam_coupling::int_aero_mmr_field_name(mode, a);
      if(not int_mmr_field_name.empty()) {
        add_tracer<Updated>(int_mmr_field_name, grid_, q_unit);
      }
    }  // end for loop num species
  }    // end for loop for num modes

  for(int g = 0; g < mam_coupling::num_aero_gases(); ++g) {
    const std::string gas_mmr_field_name = mam_coupling::gas_mmr_field_name(g);
    add_tracer<Updated>(gas_mmr_field_name, grid_, q_unit);
  }  // end for loop num gases
}

// ================================================================
void MAMGenericInterface::add_tracers_aerosol_and_gases() {
  add_tracers_interstitial_aerosol_and_gases();
  add_tracers_cloudborne_aerosol();
}

// ================================================================
void MAMGenericInterface::populate_cloudborne_wet_and_dry_aero() {
  // cloudborne aerosol tracers of interest: mass (q) and
  // number (n) mixing ratios
  for(int m = 0; m < mam_coupling::num_aero_modes(); ++m) {

    // cloudborne aerosol tracers of interest: number (n) mixing ratios
    const std::string cld_nmr_field_name =
        mam_coupling::cld_aero_nmr_field_name(m);
    wet_aero_.cld_aero_nmr[m] =
        get_field_out(cld_nmr_field_name).get_view<Real **>();
    dry_aero_.cld_aero_nmr[m] = buffer_.dry_cld_aero_nmr[m];

    for(int a = 0; a < mam_coupling::num_aero_species(); ++a) {
      // (cloudborne) aerosol tracers of interest: mass (q) mixing ratios
      const std::string cld_mmr_field_name =
          mam_coupling::cld_aero_mmr_field_name(m, a);
      if(not cld_mmr_field_name.empty()) {
        wet_aero_.cld_aero_mmr[m][a] =
            get_field_out(cld_mmr_field_name).get_view<Real **>();
        dry_aero_.cld_aero_mmr[m][a] = buffer_.dry_cld_aero_mmr[m][a];
      }
    }
  }
}
// ================================================================
void MAMGenericInterface::populate_interstitial_wet_and_dry_aero() {
  // interstitial aerosol tracers of interest: mass (q) and
  // number (n) mixing ratios
  for(int m = 0; m < mam_coupling::num_aero_modes(); ++m) {
    // interstitial aerosol tracers of interest: number (n) mixing ratios
    const std::string int_nmr_field_name =
        mam_coupling::int_aero_nmr_field_name(m);
    wet_aero_.int_aero_nmr[m] =
        get_field_out(int_nmr_field_name).get_view<Real **>();
    dry_aero_.int_aero_nmr[m] = buffer_.dry_int_aero_nmr[m];

    for(int a = 0; a < mam_coupling::num_aero_species(); ++a) {
      // (interstitial) aerosol tracers of interest: mass (q) mixing ratios
      const std::string int_mmr_field_name =
          mam_coupling::int_aero_mmr_field_name(m, a);

      if(not int_mmr_field_name.empty()) {
        wet_aero_.int_aero_mmr[m][a] =
            get_field_out(int_mmr_field_name).get_view<Real **>();
        dry_aero_.int_aero_mmr[m][a] = buffer_.dry_int_aero_mmr[m][a];
      }
    }
  }
  for(int g = 0; g < mam_coupling::num_aero_gases(); ++g) {
    const std::string gas_mmr_field_name = mam_coupling::gas_mmr_field_name(g);
    wet_aero_.gas_mmr[g] =
        get_field_out(gas_mmr_field_name).get_view<Real **>();
    dry_aero_.gas_mmr[g] = buffer_.dry_gas_mmr[g];
  }
}


// ================================================================
void MAMGenericInterface::populate_wet_and_dry_aero() {
    populate_interstitial_wet_and_dry_aero();
    populate_cloudborne_wet_and_dry_aero();
}
void MAMGenericInterface::populate_wet_and_dry_atm() {
  // store fields only to be converted to dry mmrs in wet_atm_
  wet_atm_.qv = get_field_in("qv").get_view<const Real **>();
  wet_atm_.qc = get_field_in("qc").get_view<const Real **>();
  wet_atm_.nc = get_field_in("nc").get_view<const Real **>();
  wet_atm_.qi = get_field_in("qi").get_view<const Real **>();
  wet_atm_.ni = get_field_in("ni").get_view<const Real **>();

  // store rest fo the atm fields in dry_atm_in
  dry_atm_.z_surf = 0;
  dry_atm_.T_mid  = get_field_in("T_mid").get_view<const Real **>();
  dry_atm_.p_mid  = get_field_in("p_mid").get_view<const Real **>();
  dry_atm_.p_int  = get_field_in("p_int").get_view<const Real **>();
  dry_atm_.p_del  = get_field_in("pseudo_density").get_view<const Real **>();
  dry_atm_.omega  = get_field_in("omega").get_view<const Real **>();

  // store fields converted to dry mmr from wet mmr in dry_atm_
  dry_atm_.qv = buffer_.qv_dry;
  dry_atm_.qc = buffer_.qc_dry;
  dry_atm_.nc = buffer_.nc_dry;
  dry_atm_.qi = buffer_.qi_dry;
  dry_atm_.ni = buffer_.ni_dry;

  // pbl_height
  dry_atm_.pblh = get_field_in("pbl_height").get_view<const Real *>();

  // geometric thickness of layers (m)
  dry_atm_.dz = buffer_.dz;

  // geopotential height above surface at interface levels (m)
  dry_atm_.z_iface = buffer_.z_iface;

  // geopotential height above surface at mid levels (m)
  dry_atm_.z_mid = buffer_.z_mid;

  // total cloud fraction
  dry_atm_.cldfrac = get_field_in("cldfrac_tot").get_view<const Real **>();

  // computed updraft velocity
  dry_atm_.w_updraft = buffer_.w_updraft;
}
void MAMGenericInterface::add_tracers_wet_and_dry_atm() {
  // Define the different field layouts that will be used for this process
  using namespace ShortFieldTagsNames;
  // Layout for 3D (2d horiz X 1d vertical) variable defined at mid-level and
  // interfaces
  const auto &grid_name = grid_->name();
  const int ncol =
      grid_->get_num_local_dofs();  // Number of columns on this rank
  const FieldLayout scalar3d_mid = grid_->get_3d_scalar_layout(true);
  const FieldLayout scalar3d_int = grid_->get_3d_scalar_layout(false);
  // layout for 2D (1d horiz X 1d vertical) variable
  FieldLayout scalar2d_layout_col{{COL}, {ncol}};
  using namespace ekat::units;
  constexpr auto q_unit = kg / kg;  // units of mass mixing ratios of tracers
  constexpr auto n_unit = 1 / kg;   // units of number mixing ratios of tracers

  constexpr auto nondim = ekat::units::Units::nondimensional();

  // atmospheric quantities
  // specific humidity [kg/kg]
  add_tracer<Required>("qv", grid_, q_unit);

  // cloud liquid mass mixing ratio [kg/kg]
  add_tracer<Required>("qc", grid_, q_unit);

  // cloud ice mass mixing ratio [kg/kg]
  add_tracer<Required>("qi", grid_, q_unit);

  // cloud liquid number mixing ratio [1/kg]
  add_tracer<Required>("nc", grid_, n_unit);

  // cloud ice number mixing ratio [1/kg]
  add_tracer<Required>("ni", grid_, n_unit);

  // Temperature[K] at midpoints
  add_field<Required>("T_mid", scalar3d_mid, K, grid_name);

  // Vertical pressure velocity [Pa/s] at midpoints
  add_field<Required>("omega", scalar3d_mid, Pa / s, grid_name);

  // Total pressure [Pa] at midpoints
  add_field<Required>("p_mid", scalar3d_mid, Pa, grid_name);

  // Total pressure [Pa] at interfaces
  add_field<Required>("p_int", scalar3d_int, Pa, grid_name);

  // Layer thickness(pdel) [Pa] at midpoints
  add_field<Required>("pseudo_density", scalar3d_mid, Pa, grid_name);

  // planetary boundary layer height
  add_field<Required>("pbl_height", scalar2d_layout_col, m, grid_name);

  // cloud fraction [nondimensional] computed by eamxx_cld_fraction_process
  add_field<Required>("cldfrac_tot", scalar3d_mid, nondim, grid_name);
}

void MAMGenericInterface::print_fields_names() {
  const auto &in_fields = get_fields_in();
  std::cout << "Checking interval pre for..."
            << "\n";
  for(const auto &item : in_fields) {
    auto &field_name = item.name();
    std::cout << field_name << "\n";
  }
  std::cout << "Checking interval post for..."
            << "\n";
  const auto &out_fields = get_fields_out();
  for(const auto &item : out_fields) {
    auto &field_name = item.name();
    std::cout << field_name << "\n";
  }
}
// ================================================================
void MAMGenericInterface::add_interval_checks() {
  if(check_fields_intervals_) {
    const auto &in_fields = get_fields_in();
    for(const auto &item : in_fields) {
      auto &field_name     = item.name();
      const auto ranges    = get_range(field_name);
      const auto min_value = ranges.first;
      const auto max_value = ranges.second;
      add_precondition_check<FieldWithinIntervalCheck>(
          get_field_in(field_name), grid_, min_value, max_value, false);
    }

    const auto &out_fields = get_fields_out();
    for(const auto &item : out_fields) {
      auto &field_name     = item.name();
      const auto ranges    = get_range(field_name);
      const auto min_value = ranges.first;
      const auto max_value = ranges.second;
      add_postcondition_check<FieldWithinIntervalCheck>(
          get_field_out(field_name), grid_, min_value, max_value, false);
    }
  }
}

void MAMGenericInterface::pre_process()
{
  const auto scan_policy = ekat::ExeSpaceUtils<
      KT::ExeSpace>::get_thread_range_parallel_scan_team_policy(ncol_, nlev_);

     const auto & wet_atm = wet_atm_;
     const auto & dry_atm = dry_atm_;
     const auto & wet_aero = wet_aero_;
     const auto & dry_aero  = dry_aero_;
     Kokkos::parallel_for(
      scan_policy, KOKKOS_LAMBDA(const ThreadTeam &team) {
      const int i = team.league_rank();  // column index

      mam_coupling::compute_dry_mixing_ratios(team, wet_atm, dry_atm, i);
      mam_coupling::compute_dry_mixing_ratios(team, wet_atm, wet_aero,
                                dry_aero, i);
      team.team_barrier();
      // vertical heights has to be computed after computing dry mixing ratios
      // for atmosphere
      mam_coupling::compute_vertical_layer_heights(team, dry_atm, i);
      mam_coupling::compute_updraft_velocities(team, wet_atm, dry_atm, i);
      // allows kernels below to use layer heights operator()
      team.team_barrier();
      // set_min_background_mmr(team, dry_aero_pre_,
      //                        i);  // dry_atm_pre_ is the output
    });

}

void MAMGenericInterface::post_process()
{

  const auto scan_policy = ekat::ExeSpaceUtils<
      KT::ExeSpace>::get_thread_range_parallel_scan_team_policy(ncol_, nlev_);

     const auto & dry_atm = dry_atm_;
     const auto & dry_aero  = dry_aero_;
     const auto & wet_aero = wet_aero_;
     Kokkos::parallel_for(
      scan_policy, KOKKOS_LAMBDA(const ThreadTeam &team) {
      const int i = team.league_rank();  // column index
      compute_wet_mixing_ratios(team, dry_atm, dry_aero,
                                wet_aero, i);
      });

}
}  // namespace scream
