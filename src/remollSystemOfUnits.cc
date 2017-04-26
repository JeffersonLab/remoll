#include "remollSystemOfUnits.hh"
#include "remolltypes.hh"

remollUnits_t::remollUnits_t():
    // Asymmetry
    ppm (remoll::ppm),
    ppb (remoll::ppb),
    // Distance
    nm  (CLHEP::nm),
    um  (CLHEP::um),
    mm  (CLHEP::mm),
    cm  (CLHEP::cm),
    m   (CLHEP::m),
    // Area
    mm2 (CLHEP::mm2),
    cm2 (CLHEP::cm2),
    m2  (CLHEP::m2),
    // Volume
    mm3 (CLHEP::mm3),
    cm3 (CLHEP::cm3),
    m3  (CLHEP::m3),
    // Energy
    eV (CLHEP::eV),
    keV (CLHEP::keV),
    MeV (CLHEP::MeV),
    GeV (CLHEP::GeV),
    // Angle
    rad (CLHEP::rad),
    deg (CLHEP::deg),
    sr  (CLHEP::sr),
    // Cross Section
    barn  (CLHEP::barn),
    mbarn (CLHEP::millibarn),
    ubarn (CLHEP::microbarn),
    // Time
    nsec (CLHEP::ns),
    msec (CLHEP::ms),
    sec  (CLHEP::s),
    // Frequency
    Hz  (1.0/CLHEP::s),
    kHz (Hz/1000.0),
    MHz (kHz/1000.0),
    GHz (MHz/1000.0)
{ };

