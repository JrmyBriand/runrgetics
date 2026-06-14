#' @importFrom utils globalVariables
utils::globalVariables(c(
  "time", "velocity", "acceleration", "cost_running",
  "metabolic_power", "external_power", "external_mechanical_efficiency"
))

#' External (mechanical) running power
#'
#' Instantaneous external mechanical power per unit body mass from acceleration
#' and velocity, following di Prampero et al. (2024), *Mechanical and Metabolic
#' Power in Accelerated Running - PART I: the 100-m dash*.
#'
#' @param acceleration Numeric vector of accelerations (m/s^2).
#' @param velocity Numeric vector of velocities (m/s).
#'
#' @returns A numeric vector of external mechanical power (W/kg).
#' @export
#'
#' @examples
#' external_power(acceleration = c(0, 2, 4), velocity = c(2, 4, 6))
external_power <- function(acceleration, velocity) {
  # constant external work

  w_est_const <- 1.3 - 0.4 / velocity

  external_power <- w_est_const * velocity + acceleration * velocity # check units from
  # from di Prampero et al. 2024 Mechanical and Metabolic Power in Accelerated Running–PART I:
  # the 100‑m dash

  return(external_power)
}


#' Compute external running power over a run
#'
#' Smooths velocity (optionally), computes acceleration and the instantaneous
#' external mechanical power ([external_power()]) for a running data frame.
#'
#' @param running_data_frame A data frame with `time` (s) and `velocity` (m/s) columns.
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#'
#' @returns The input data frame with `acceleration` (m/s^2) and `external_power`
#'   (W/kg) columns added.
#' @export
#'
#' @examples
#' run <- tibble::tibble(time = 0:9,
#'                       velocity = c(0, 2, 4, 6, 7, 7.5, 7.6, 7.6, 7.5, 7.4))
#' compute_external_running_power(run)
compute_external_running_power <- function(running_data_frame,
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
      external_power = external_power(acceleration, velocity)
    ) |>
    dplyr::ungroup()

  return(running_data_power)
}




#' Compute external mechanical efficiency over a run
#'
#' Computes external power and metabolic power for a run and their ratio, the
#' external mechanical efficiency, keeping only physically admissible rows
#' (efficiency in the half-open interval (0, 1]).
#'
#' @param running_data_frame A data frame with `time` (s) and `velocity` (m/s) columns.
#' @param cost_running_flat Flat-terrain cost of running (J/kg/m); see [cost_running()].
#' @param slope_equation Slope equation passed to [cost_running()] (`"original"` or `"extended"`).
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#'
#' @returns The input data frame with `acceleration`, `cost_running`,
#'   `metabolic_power`, `external_power` and `external_mechanical_efficiency`
#'   columns, filtered to admissible efficiencies.
#' @export
#'
#' @examples
#' run <- tibble::tibble(time = 0:9,
#'                       velocity = c(0, 2, 4, 6, 7, 7.5, 7.6, 7.6, 7.5, 7.4))
#' compute_external_mechanical_efficiency(run)
compute_external_mechanical_efficiency <- function(running_data_frame,
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
      metabolic_power = cost_running * velocity,
      external_power = external_power(acceleration, velocity),
      external_mechanical_efficiency = external_power / metabolic_power
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      external_mechanical_efficiency <= 1,
      external_mechanical_efficiency > 0
    ) # necessary conditions, to be validated

  return(running_data_power)
}


#' Compute external running power from an activity file
#'
#' Reads an activity file (`.fit` or `.csv`) and computes external running power
#' with [compute_external_running_power()].
#'
#' @param file_path Path to the activity file.
#' @param file_type Either `"fit"` (default) or `"csv"`.
#' @param smoothing Velocity smoothing method passed to [smooth_velocity()].
#' @param window Smoothing window length (samples).
#' @param order Smoothing / Butterworth order.
#' @param cfreq Butterworth normalised cut-off.
#'
#' @returns A data frame with external power columns (see [compute_external_running_power()]).
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) compute_activity_external_running_power(f)
#' }
compute_activity_external_running_power <- function(file_path,
                                            file_type = "fit",
                                            smoothing = "none",
                                            window = 3,
                                            order = 2,
                                            cfreq = 0.1) {
  if (file_type == "csv") {
    run_data <- readr::read_csv(file_path)

    external_power_data <- compute_external_running_power(run_data,
      smoothing = smoothing,
      window = window,
      order = order,
      cfreq = cfreq
    )

    return(external_power_data)
  }

  if (file_type == "fit") {
    fit_data <- get_fit_data_frame(file_path)

    run_data <- reformat_fit_data_frame(fit_data)

    external_power_data <- compute_external_running_power(run_data,
      smoothing = smoothing,
      window = window,
      order = order,
      cfreq = cfreq
    )

    return(external_power_data)
  }

  stop("Unsupported file type.")
}


#' Compute external power and efficiency from an activity file
#'
#' Reads an activity file (`.fit` or `.csv`) and computes external power, metabolic
#' power and external mechanical efficiency with
#' [compute_external_mechanical_efficiency()].
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
#' @returns A data frame with external power, metabolic power and efficiency columns
#'   (see [compute_external_mechanical_efficiency()]).
#' @export
#'
#' @examples
#' \donttest{
#' f <- system.file("extdata/activities/sample_run.fit", package = "runrgetics")
#' if (nzchar(f)) compute_activity_power_efficiency_data(f)
#' }
compute_activity_power_efficiency_data <- function(file_path,
                                                   file_type = "fit",
                                                   cost_running_flat = 3.6,
                                                   slope_equation = "extended",
                                                   smoothing = "none",
                                                   window = 3,
                                                   order = 2,
                                                   cfreq = 0.1) {
  if (file_type == "csv") {
    run_data <- readr::read_csv(file_path)

    power_data <- compute_external_mechanical_efficiency(run_data,
      cost_running_flat = cost_running_flat,
      slope_equation = slope_equation,
      smoothing = smoothing,
      window = window,
      order = order,
      cfreq = cfreq
    )

    return(power_data)
  }

  if (file_type == "fit") {
    fit_data <- get_fit_data_frame(file_path)

    run_data <- reformat_fit_data_frame(fit_data)

    power_data <- compute_external_mechanical_efficiency(run_data,
      cost_running_flat = cost_running_flat,
      slope_equation = slope_equation,
      smoothing = smoothing,
      window = window,
      order = order,
      cfreq = cfreq
    )

    return(power_data)
  }

  stop("Unsupported file type.")
}
