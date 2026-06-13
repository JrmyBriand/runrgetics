#' Graubner and Nixdorf sprint data
#'
#' Time splits over distances recorded at the 2009 World athletics Championships in Berlin.
#'
#' @format ## `graubner_nixdorf_sprints`
#' A tibble with 45 rows and 6 columns:
#' \describe{
#'   \item{distance}{Distance (in m) at which time splits were measured for each event}
#'   \item{splits}{Time splits (in s)}
#'   \item{velocity}{Average velocity (m/s) over each distance interval}
#'   \item{reaction_time}{Reaction time (s) measured on the starting blocks at the beginning of the race}
#'   \item{maximal_velocity}{Maximal velocity (m/s) measured during the race. If not available, NA}
#'   \item{athlete}{Name of the athlete}
#'   \item{event}{Race event at the World athletics championships}
#' }
#' @source Graubner, R., & Nixdorf, E. (2009). Biomechanical analysis of the sprint and hurdles events at the 2009 IAAF World Championships in Athletics. Positions, 1(10).
"graubner_nixdorf_sprints"


#' Kindermann lactate data
#'
#' Provides lactate and accumulated lactate values over running durations
#'
#' @format ## `kindermann_lactate`
#' A tibble with 10 rows and 3 columns:
#' \describe{
#'   \item{duration}{Duration (in s) of the running event over which lactate was measured}
#'   \item{lactate}{Blood lactate concentration (in mmol/L)}
#'   \item{accumulated_lactate}{Accumulated lactate above blood lactate resting levels, which are assumed to be 1 mmol/L}
#' }
#' @source Kindermann, W. (1977). Lactate acidosis with different forms of sports activities. Can. J. Appl. Sports Sci. https://cir.nii.ac.jp/crid/1574231873976904704

"kindermann_lactate"


#' Paired-device sprint-mix session
#'
#' A single mixed-sprint track session recorded **simultaneously** by two devices:
#' a gpexe local-positioning unit (~25 Hz, downsampled here to 5 Hz; treated as the
#' reference signal thanks to its onboard filtering) and a Polar/Stryd watch (~1 Hz,
#' GPS, noisier). The two recordings were trimmed (first and last 5 min removed),
#' time-synchronized and spatially aligned on the start-line standstill that precedes
#' the first sprint, then stacked into one long (tidy) frame with a `source` column.
#' The device clock offset was estimated by cross-correlating the two velocity signals
#' over the whole session; the watch positions were rigid-shifted onto the gpexe
#' start-line origin so the GPS tracks overlay. Full-resolution raw recordings are
#' shipped under `inst/extdata` (`sprint_mix_gpexe.csv.gz`, `sprint_mix_polar_stryd.fit`).
#'
#' @format ## `sprint_mix_paired`
#' A tibble with 19,823 rows and 15 columns (16,547 gpexe rows at 5 Hz, 3,276
#' Polar/Stryd rows at 1 Hz). Columns absent for a given device are `NA`:
#' \describe{
#'   \item{source}{Recording device: `"gpexe"` (5 Hz reference) or `"polar_stryd"` (1 Hz watch)}
#'   \item{time}{Synchronized time (s); `0` = athlete leaving the start line into the first sprint. Negative during the warm-up}
#'   \item{datetime_utc}{Original sample timestamp (`POSIXct`, UTC) on that device's own clock (not offset-corrected; use `time` for the cross-device axis)}
#'   \item{velocity}{Speed (m/s): gpexe filtered speed, or watch speed}
#'   \item{latitude}{Latitude (WGS84 degrees). Watch coordinates rigid-shifted onto the gpexe start-line origin; gpexe native}
#'   \item{longitude}{Longitude (WGS84 degrees), aligned as for `latitude`}
#'   \item{elapsed_s}{Device-native elapsed time (s) since that device's recording start, before synchronization}
#'   \item{power_w_kg}{gpexe running power (W/kg); `NA` for the watch}
#'   \item{external_power_positive}{gpexe positive external power (W/kg); `NA` for the watch}
#'   \item{raw_speed}{gpexe raw (unfiltered) speed (m/s); `NA` for the watch}
#'   \item{acc}{gpexe acceleration (m/s^2); `NA` for the watch}
#'   \item{power_w}{Stryd running power in **absolute watts** (not W/kg); `NA` for gpexe}
#'   \item{heart_rate}{Watch heart rate (bpm); `NA` for gpexe}
#'   \item{cadence}{Watch running cadence; `NA` for gpexe}
#'   \item{altitude}{Watch altitude (m); `NA` for gpexe}
#' }
#' @source Simultaneous gpexe and Polar/Stryd recording of one mixed-sprint track
#'   session (Montreal, 2026). Prepared by `data-raw/paired_device_sessions.R`.
"sprint_mix_paired"


#' Paired-device 10 x 200 m sprint session
#'
#' A 10 x 200 m repeated-sprint track session recorded **simultaneously** by a gpexe
#' unit (~25 Hz, downsampled here to 5 Hz; reference signal) and a Polar/Stryd watch
#' (~1 Hz, GPS, noisier). Processing is identical to [sprint_mix_paired]: 5-min
#' head/tail trim, time synchronization and spatial alignment on the start-line
#' standstill before the first sprint, then merged into one long frame with a `source`
#' column. Full-resolution raw recordings are shipped under `inst/extdata`
#' (`ten_200_sprints_gpexe.csv.gz`, `ten_200_sprints_polar_stryd.fit`).
#'
#' @format ## `ten_200_sprints_paired`
#' A tibble with 20,108 rows and 15 columns (gpexe at 5 Hz, Polar/Stryd at 1 Hz);
#' same columns as [sprint_mix_paired]. Columns absent for a given device are `NA`:
#' \describe{
#'   \item{source}{Recording device: `"gpexe"` (5 Hz reference) or `"polar_stryd"` (1 Hz watch)}
#'   \item{time}{Synchronized time (s); `0` = athlete leaving the start line into the first sprint. Negative during the warm-up}
#'   \item{datetime_utc}{Original sample timestamp (`POSIXct`, UTC) on that device's own clock (not offset-corrected; use `time` for the cross-device axis)}
#'   \item{velocity}{Speed (m/s): gpexe filtered speed, or watch speed}
#'   \item{latitude}{Latitude (WGS84 degrees). Watch coordinates rigid-shifted onto the gpexe start-line origin; gpexe native}
#'   \item{longitude}{Longitude (WGS84 degrees), aligned as for `latitude`}
#'   \item{elapsed_s}{Device-native elapsed time (s) since that device's recording start, before synchronization}
#'   \item{power_w_kg}{gpexe running power (W/kg); `NA` for the watch}
#'   \item{external_power_positive}{gpexe positive external power (W/kg); `NA` for the watch}
#'   \item{raw_speed}{gpexe raw (unfiltered) speed (m/s); `NA` for the watch}
#'   \item{acc}{gpexe acceleration (m/s^2); `NA` for the watch}
#'   \item{power_w}{Stryd running power in **absolute watts** (not W/kg); `NA` for gpexe}
#'   \item{heart_rate}{Watch heart rate (bpm); `NA` for gpexe}
#'   \item{cadence}{Watch running cadence; `NA` for gpexe}
#'   \item{altitude}{Watch altitude (m); `NA` for gpexe}
#' }
#' @source Simultaneous gpexe and Polar/Stryd recording of one 10 x 200 m
#'   repeated-sprint track session (Montreal, 2026). Prepared by
#'   `data-raw/paired_device_sessions.R`.
"ten_200_sprints_paired"
