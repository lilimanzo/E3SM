#ifndef EAMXX_MAM_SRF_ONLINE_EMISS_HPP
#define EAMXX_MAM_SRF_ONLINE_EMISS_HPP

// For declaring surface and online emission class derived from atm process class
#include <share/atm_process/atmosphere_process.hpp>

// For MAM4 aerosol configuration
#include <physics/mam/mam_coupling.hpp>

// For component name
#include <string>

namespace scream {

// The process responsible for handling MAM4 surface and online emissions. The AD
// stores exactly ONE instance of this class in its list of subcomponents.
class MAMSrfOnlineEmiss final : public scream::AtmosphereProcess {
  // number of horizontal columns and vertical levels
  int ncol_, nlev_;

  // buffer for sotring temporary variables
  mam_coupling::Buffer buffer_;

  // physics grid for column information
  std::shared_ptr<const AbstractGrid> grid_;

 public:
  // Constructor
  MAMSrfOnlineEmiss(const ekat::Comm &comm, const ekat::ParameterList &params);

  // --------------------------------------------------------------------------
  // AtmosphereProcess overrides (see share/atm_process/atmosphere_process.hpp)
  // --------------------------------------------------------------------------

  // The type of subcomponent
  AtmosphereProcessType type() const { return AtmosphereProcessType::Physics; }

  // The name of the subcomponent
  std::string name() const { return "mam_srf_online_emissions"; }

  // grid
  void set_grids(
      const std::shared_ptr<const GridsManager> grids_manager) override;

  // management of common atm process memory
  size_t requested_buffer_size_in_bytes() const override;
  void init_buffers(const ATMBufferManager &buffer_manager) override;

  // Initialize variables
  void initialize_impl(const RunType run_type) override;

  // Run the process by one time step
  void run_impl(const double dt) override;

  // Finalize
  void finalize_impl(){/*Do nothing*/};

};  // MAMSrfOnlineEmiss

}  // namespace scream

#endif  // EAMXX_MAM_SRF_ONLINE_EMISS_HPP