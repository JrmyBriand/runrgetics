# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

```r
# Build and check package
devtools::check()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-sprint_bioenergetic_model.R")

# Regenerate documentation
devtools::document()

# Build vignettes
devtools::build_vignettes()

# Install locally
devtools::install()
```

## Package Architecture

runrgetics implements the sprint bioenergetic model from Briand et al. (2025) to quantify energy contributions from three metabolic systems during running:

**Energy Systems Modeling:**
- **Alactic (phosphocreatine)**: Log-normal power distribution → `sprint_alactic_energy_duration.R`
- **Lactic (glycolytic)**: Bi-exponential function → `sprint_lactic_energy_duration.R`
- **Aerobic**: Exponential saturation → integrated in `sprint_bioenergetic_model.R`

**Core Computation Flow:**
1. Motion data (velocity/acceleration) → `sprint_motion_computations.R`, `sprint_recover_motion_data.R`
2. Running power calculation → `cost_of_running.R`, `metabolic_conversions.R`
3. Energy pathway decomposition → `sprint_bioenergetic_model.R`
4. Capacity extraction → `sprint_extract_capacities_from_athlete_perf.R`

**Key R/ Files by Domain:**
- `sprint_bioenergetic_model.R` - Main model integrating all energy systems
- `sprint_motion_computations.R` - Velocity, acceleration, distance calculations
- `cost_of_running.R` - Energy cost from di Prampero methodology
- `energy_integrals.R` - Total energy expenditure integration
- `sprint_performance_simulation.R` - Simulate athlete performances from parameters
- `plot_*.R` - Visualization functions for each model component

## Code Style

- Use tidyverse functions with `|>` pipe
- All exported functions have roxygen2 documentation
- Tests follow `test-{source_file}.R` naming convention
