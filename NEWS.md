# runrgetics 0.0.0.9000

## New data

* Two paired-device track sessions recorded simultaneously by a gpexe unit and a
  Polar/Stryd watch, time-synchronised and spatially aligned on the start-line
  standstill: `sprint_mix_paired` and `ten_200_sprints_paired`. Full-resolution raw
  recordings ship gzipped under `inst/extdata`.

## New features

### Watch GPS filtering

* `filter_watch_motion()` low-pass filters noisy watch speed/position to approximate
  the gpexe-filtered reference (zero-phase Butterworth by default, tuned against the
  paired data). `gps_speed()`, `motion_agreement()` and `tune_watch_filter()` support
  deriving GPS speed, quantifying agreement, and re-tuning on new paired sessions.

### Device comparison

* `compare_devices()` / `plot_device_comparison()` compare the filtered watch against
  the gpexe reference across speed, acceleration, distance, external and metabolic power.
* `compare_sprint_power_sources()` / `plot_sprint_power_sources()` give a per-sprint
  4-panel comparison of the watch against gpexe (or its raw-GPS-derived speed), with the
  Stryd-measured external power overlaid (`body_mass` parameter).

### Sprint detection and workout comparison

* `detect_sprints()` auto-detects high-intensity efforts (speed/power/acceleration
  threshold).
* `compare_workout_sprints()` / `plot_workout_sprints()` summarise and overlay the
  efforts of a workout (speed-vs-distance and other channels); `sprint_ids` selects a
  subset.

### Training bioenergetics and energy

* `analyze_training_bioenergetics()` / `analyze_sprint_bioenergetics()` decompose each
  sprint's metabolic power into alactic / lactic / aerobic contributions, with the
  end-of-effort deceleration auto-trimmed (`trim_sprint_deceleration()`,
  `trim_sprint_launch()`) and `maximal_aerobic_power` (MAP) as a parameter.
  `plot_sprint_bioenergetics()` plots the decomposition.
* `sprint_bioenergetic_model_fit()` gains `fit_mu`, `fit_sigma` and `fit_k2` to
  optionally estimate the alactic peak location/width and the lactic decay; the gpexe
  training functions enable these by default for slower training sprints.
* `sprint_energy_data()`, `plot_sprint_energy()` and `plot_workout_energy()` integrate
  pathway power into cumulative energy (J/kg) versus distance, as stacked areas per
  sprint and superposed across a workout (`sprint_ids` for a subset).

### Session analysis and dashboard

* `read_gpexe_csv()` reads a gpexe CSV export into a tidy motion data frame, and
  `analyze_sprint_session()` runs the whole pipeline (detection, workout comparison,
  bioenergetic decomposition) on one session in a single call.
* `batch_sprint_analysis()` processes a folder of gpexe sessions, writing per-session
  tables and figures plus a run log; files that error are logged and skipped.
* `launch_sprint_dashboard()` starts an interactive Shiny (shinydashboard) app for
  coaches to browse a folder of sessions and explore sprint kinematics and the
  bioenergetic partition (requires the suggested `shiny`, `shinydashboard`, `DT`).

### Shared plotting style

* `theme_runrgetics()`, `runrgetics_pal()` and `scale_colour_runrgetics()` give the new
  figures a consistent, colour-blind-safe `theme_classic()` look.
