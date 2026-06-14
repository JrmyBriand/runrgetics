#' @importFrom utils globalVariables
utils::globalVariables(c(
  "timestamp", "time", "velocity", "speed", "heart_rate",
  "dt", "dv", "acceleration", "cost_running", "metabolic_power"
))

#' Build a data frame from a FIT file
#'
#' Reads a `.fit` activity file with FITfileR and returns its record messages as
#' a single data frame.
#'
#' @param fit_file Path to a `.fit` file.
#'
#' @returns A data frame of the FIT record messages (one row per recorded sample).
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) get_fit_data_frame(f)
#' }
get_fit_data_frame <- function(fit_file) {
  fit_data <- FITfileR::readFitFile(fit_file)

  fit_records <- FITfileR::records(fit_data)

  # combine fit records

  fit_data_frame <- combine_fit_records(fit_records)

  return(fit_data_frame)
}


#' Combine FIT record messages
#'
#' Row-binds the (possibly several) record-message tables returned by
#' [FITfileR::records()] into a single data frame.
#'
#' @param fit_records_list A list of FIT record data frames (or a single data frame).
#'
#' @returns A single data frame combining all record messages.
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) {
#'   recs <- FITfileR::records(FITfileR::readFitFile(f))
#'   combine_fit_records(recs)
#' }
#' }
combine_fit_records <- function(fit_records_list) {
  combined_records <- dplyr::bind_rows(fit_records_list)

  return(combined_records)
}

#' Reformat a FIT data frame for power computation
#'
#' Converts the FIT `timestamp` to an elapsed `time` (s), renames `speed` to
#' `velocity` and `heart_rate` to `heartrate`, and moves `time` to the front.
#'
#' @param fit_data_frame A FIT data frame as returned by [get_fit_data_frame()],
#'   containing `timestamp`, `speed` and `heart_rate` columns.
#'
#' @returns A data frame with a numeric `time` (s) column, `velocity` (m/s) and
#'   `heartrate`, ready for [compute_metabolic_running_power()].
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) reformat_fit_data_frame(get_fit_data_frame(f))
#' }
reformat_fit_data_frame <- function(fit_data_frame) {
  data <- fit_data_frame |>
    dplyr::mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) |>
    dplyr::mutate(time = as.numeric(difftime(timestamp, min(timestamp), units = "secs"))) |>
    dplyr::select(-timestamp) |>
    dplyr::rename(
      velocity = speed,
      heartrate = heart_rate
    ) |>
    dplyr::select(time, everything())


  return(data)
}


#' Compute acceleration from a velocity series
#'
#' Adds an `acceleration` (m/s^2) column to a running data frame as the finite
#' difference of `velocity` over `time`, interpolating internal `NA`s.
#'
#' @param running_data_frame A data frame with `time` (s) and `velocity` (m/s) columns.
#'
#' @returns The input data frame with `dt`, `dv` and `acceleration` (m/s^2) columns added.
#' @export
#'
#' @examples
#' run <- tibble::tibble(time = 0:5, velocity = c(0, 2, 4, 5, 5.5, 5.5))
#' compute_acceleration(run)
compute_acceleration <- function(running_data_frame) {
  running_data_frame <- running_data_frame %>%
    mutate(
      dt = time - lag(time),
      dv = velocity - lag(velocity),
      acceleration = dv / dt
    ) %>%
    mutate(acceleration = zoo::na.approx(acceleration, na.rm = FALSE, rule = 2)) # interpolate internal NAs
  return(running_data_frame)
}




#' Smooth a velocity series
#'
#' Optional smoothing of a velocity vector by moving average, exponential moving
#' average or a zero-phase Butterworth low-pass. For watch GPS data tuned against
#' the gpexe reference, see [filter_watch_motion()].
#'
#' @param velocity Numeric vector of velocities (m/s).
#' @param method One of `"none"`, `"moving_average"`,
#'   `"exponential_moving_average"` or `"butterworth"`.
#' @param time Optional numeric vector of times (s); kept for interface consistency.
#' @param window Window length (samples) for the moving-average methods.
#' @param order Butterworth filter order.
#' @param cfreq Butterworth normalised cut-off (fraction of the Nyquist frequency).
#'
#' @returns A numeric vector of smoothed velocities (m/s).
#' @export
#'
#' @examples
#' smooth_velocity(c(0, 2, 3, 2, 4, 5), method = "moving_average", window = 3)
smooth_velocity <- function(velocity, method = "none", time = NULL, window = 3, order = 2, cfreq = 0.1) {
  n <- length(velocity)

  if (method == "none") {
    return(velocity)
  }

  if (method == "moving_average") {
    return(zoo::rollmean(velocity, k = window, fill = NA, align = "right"))
  }

  if (method == "exponential_moving_average") {
    return(as.numeric(TTR::EMA(velocity, n = window)))
  }

  if (method == "butterworth") {
    bf <- signal::butter(n = order, W = cfreq, type = "low")
    return(signal::filtfilt(bf, velocity))
  }

  stop("Unsupported smoothing method.")
}



#' Compute metabolic running power over a run
#'
#' Smooths velocity (optionally), computes acceleration and the instantaneous
#' metabolic running power as the cost of running times velocity, using the sprint
#' cost-of-running model ([cost_running()]).
#'
#' @param running_data_frame A data frame with `time` (s) and `velocity` (m/s) columns.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m); see [cost_running()].
#' @param slope_equation Slope equation passed to [cost_running()] (`"original"` or `"extended"`).
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#'
#' @returns The input data frame with `acceleration` (m/s^2), `cost_running`
#'   (J/kg/m) and `metabolic_power` (W/kg) columns added.
#' @export
#'
#' @examples
#' run <- tibble::tibble(time = 0:9,
#'                       velocity = c(0, 2, 4, 6, 7, 7.5, 7.6, 7.6, 7.5, 7.4))
#' compute_metabolic_running_power(run)
compute_metabolic_running_power <- function(running_data_frame,
                                            cost_running_flat = 3.6,
                                            slope_equation = "extended",
                                            smoothing = "none",
                                            window = 3,
                                            order = 2,
                                            cfreq = 0.1) {
  # Smooth velocity first
  running_data_frame <- running_data_frame |>
    mutate(
      velocity = smooth_velocity(velocity, method = smoothing, window = window, time = time, order = order, cfreq = cfreq)
    )

  # Compute acceleration and metabolic power
  running_data_power <- running_data_frame |>
    compute_acceleration() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      cost_running = cost_running(acceleration, velocity, cost_running_flat, slope_equation),
      metabolic_power = cost_running * velocity
    ) |>
    dplyr::ungroup()

  return(running_data_power)
}



#' Compute metabolic running power from an activity file
#'
#' Reads an activity file (`.fit` or `.csv`) and computes metabolic running power
#' with [compute_metabolic_running_power()].
#'
#' @param file_path Path to the activity file.
#' @param file_type Either `"fit"` (default) or `"csv"`.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m).
#' @param slope_equation Slope equation passed to [cost_running()].
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#'
#' @returns A data frame with metabolic power columns (see [compute_metabolic_running_power()]).
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) compute_activity_metabolic_running_power(f)
#' }
compute_activity_metabolic_running_power <- function(file_path,
                                                     file_type = "fit",
                                                     cost_running_flat = 3.6,
                                                     slope_equation = "extended",
                                                     smoothing = "none",
                                                     window = 3,
                                                     order = 2,
                                                     cfreq = 0.1) {
  if (file_type == "csv") {
    run_data <- readr::read_csv(file_path)

    metabolic_power_data <- compute_metabolic_running_power(run_data,
      cost_running_flat = cost_running_flat,
      slope_equation = slope_equation,
      smoothing = smoothing,
      window = window,
      order = order,
      cfreq = cfreq
    )

    return(metabolic_power_data)
  }

  if (file_type == "fit") {
    fit_data <- get_fit_data_frame(file_path)

    run_data <- reformat_fit_data_frame(fit_data)

    metabolic_power_data <- compute_metabolic_running_power(run_data,
      cost_running_flat = cost_running_flat,
      slope_equation = slope_equation,
      smoothing = smoothing,
      window = window,
      order = order,
      cfreq = cfreq
    )

    return(metabolic_power_data)
  }

  stop("Unsupported file type.")
}
