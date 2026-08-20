# =============================================================================
# GASTIN (2001) CASE FIGURES -- Figs. 4 and 5
#
# Sports Med 31:725-741. The three energy systems against time for individual
# protocols, in mL O2 equivalent / kg / min. These are the CASE figures; the
# duration-vs-share summary of Fig. 3 lives in gastin.R.
#
# PROVENANCE. Digitised point by point from the paper rendered at 400 dpi, using
# data-raw/digitize_gastin_cases.R -- all four curves of all four cases clicked
# individually, not read by eye. GLYCOLYSIS is still derived here as
# demand - aerobic - ATP-PCr so the balance closes exactly; the separately
# digitised glycolysis agrees with that to 1.6-3.4 mL O2/kg/min on average,
# which is the honest error bar on the whole reading.
#
# CHECKED against a number Gastin publishes independently of the curves: the O2
# deficit of each Fig. 5 case. Digitised, they come out at 66.3 (published 65.8)
# and 72.8 (published 69.2). See the test.
#
# UNITS AND BASELINE. The figures are in mL O2 equivalent / kg / min and INCLUDE
# resting metabolism. The model works above rest and in W/kg, so gastin_case()
# subtracts vo2_rest from the demand and the aerobic curve -- not from the two
# anaerobic ones, which carry no resting component -- and consumers convert with
# EQ_O2/60. Subtracting from both demand and aerobic leaves the balance intact.
#
# Treat these as a calibration TARGET accurate to a few mL O2/kg/min, not data.
# =============================================================================

.g_curve <- function(t, v) function(tt) stats::approx(t, v, tt, rule = 2)$y

GASTIN_CASES <- list(

  fig4_sprint = list(
    label = "Fig. 4 — sprint-trained, 90 s all-out",
    VO2max = 58, duration = 90,
    atp_pcr = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      c(10.7, 79.8, 119.1, 111.9, 83.5, 59.2, 39.6, 30.2, 22.1, 14.8, 11, 8.7, 6.3, 5, 3.7, 2.8, 2.2, 1.6, 0.9, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)),
    glyco = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      c(2.6, 4.1, 14.7, 32.3, 56.6, 84.8, 99.7, 109.9, 115.7, 118.5, 117.6, 114.7, 109.2, 104.1, 100.8, 94.5, 91, 85.7, 81.2, 77.3, 72.6, 65.1, 60.4, 54.7, 49.3, 45.6, 41.3, 36.8, 32.2, 28.3, 25.6, 19.6, 15.4, 13.1, 11.2, 9.7, 8.7, 8, 6.7, 6.1, 5.6)),
    aerobic = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      c(10.2, 12, 14.2, 16.6, 19.2, 21.5, 23.5, 25.5, 27.7, 30.1, 31.7, 33, 34.4, 35.3, 36.1, 36.9, 37.6, 38.3, 39, 39.8, 40.9, 43, 43.8, 44.5, 45.2, 46, 46.9, 47.8, 48.6, 48.6, 48.6, 49, 49.1, 50.1, 50.6, 50.6, 51.1, 51.1, 51.1, 51.6, 52.2))
  ),

  fig4_endurance = list(
    label = "Fig. 4 — endurance-trained, 90 s all-out",
    VO2max = 65, duration = 90,
    atp_pcr = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 88.5),
      c(13.3, 90.1, 88.7, 68.8, 56.3, 38.6, 23.6, 16.8, 11.9, 7.6, 5, 3.2, 1.9, 1.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    glyco = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 88.5),
      c(2.6, 4.3, 14.3, 31.3, 51.7, 67.2, 77.3, 89.6, 93.4, 94.7, 95.1, 93.6, 91, 86.8, 83.4, 80.3, 77.6, 74.8, 71.2, 67.6, 64.8, 59.3, 53.9, 48.2, 43.2, 39.5, 36.1, 32.7, 29.7, 27.1, 24.6, 19.1, 15.3, 11.9, 9.7, 8.3, 6.8, 5.7, 4.7, 4, 3.6)),
    aerobic = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 88.5),
      c(7.7, 9.2, 11.1, 14, 16.6, 18.4, 20.1, 21.8, 24, 26.1, 28, 29.4, 30.8, 32.1, 33.4, 34.5, 35.5, 36.5, 37.8, 39.1, 40.4, 42.7, 44.6, 45.7, 46.8, 47.6, 49.4, 51.3, 51.5, 51.8, 52.2, 53.5, 55, 56.3, 57.1, 57.8, 58.1, 58.3, 58.3, 58.3, 58.8))
  ),

  fig5_allout = list(
    label = "Fig. 5 — 90 s all-out (VO2max 64.9)",
    VO2max = 64.9, duration = 90, o2_deficit = 65.8,
    atp_pcr = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      c(19, 20, 43, 71.4, 81, 87.1, 82.4, 61.9, 38.3, 27.3, 15.6, 10.6, 5.9, 4, 2, 1.1, 0.8, 0.4, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    glyco = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      c(0.4, 0.4, 1.7, 2.9, 6.7, 17.6, 32.7, 52.4, 77, 90.1, 101.9, 104.3, 104.9, 104.2, 103.2, 99.9, 96.8, 94, 91.2, 87, 81.4, 76.2, 69.8, 65.9, 61.7, 57.6, 53.3, 48.3, 45.7, 43.2, 40.8, 34, 28.5, 22.9, 18.9, 16.1, 13.4, 12, 9.9, 8.2, 6.6)),
    aerobic = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      c(6, 7.1, 9.1, 11.1, 13.1, 14.8, 16.5, 18.2, 19.6, 20.9, 22.2, 23.5, 24.9, 26.2, 27.6, 28.9, 30.2, 31.4, 32.6, 33.8, 34.6, 36, 37.5, 39, 40.7, 41.8, 42.8, 43.8, 45, 46.2, 47, 48.2, 49.7, 50.9, 51.5, 52, 52, 52.5, 52.6, 53.1, 53.6))
  ),

  fig5_110 = list(
    label = "Fig. 5 — 110% VO2max constant (VO2max 64.9)",
    VO2max = 64.9, duration = 205, o2_deficit = 69.2,
    atp_pcr = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 204.8),
      c(4.5, 5.2, 16, 27, 32.3, 34.4, 34.6, 27.2, 22, 17, 13.4, 10.9, 8.6, 6.5, 4.6, 3.6, 3.1, 2.5, 2.1, 1.8, 1.4, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9)),
    glyco = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 204.8),
      c(4.5, 5.4, 7.2, 8.9, 10.7, 20.8, 27.7, 31.7, 34.5, 37, 39.6, 41, 42.4, 43.8, 44.7, 45.3, 45.9, 46.2, 45.7, 45.2, 44.7, 40.6, 38, 34.3, 31.2, 28.8, 27, 25.6, 24.5, 23.1, 21.6, 19.4, 18.8, 17.9, 16.7, 15.4, 15, 14.2, 14.1, 13.5, 13.3, 12.6, 11.1, 8.4, 4.7, 2.9)),
    aerobic = .g_curve(
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 204.8),
      c(4.5, 4.6, 6.5, 8.4, 10.3, 12.3, 13.6, 14.7, 15.7, 16.8, 17.9, 19, 20.2, 21.5, 22.7, 24, 25.2, 26.5, 27.3, 28.1, 28.8, 31.9, 34.8, 37.5, 39.9, 42, 43.9, 45.1, 46.3, 47.6, 49, 51.9, 54.1, 54.5, 55.2, 55.9, 56.4, 56.9, 57.4, 58, 58.2, 58.2, 58.4, 59, 59.3, 59.3))
  )
)

#' Evaluate one Gastin case on a time grid
#'
#' @param case Name in [GASTIN_CASES].
#' @param dt Grid step (s).
#' @return data.frame(time, demand, atp_pcr, glyco, aerobic) in mL O2/kg/min,
#'   above rest. All THREE supplies are as digitised; `demand` is their sum, so
#'   the balance closes exactly and the model is not being asked to match three
#'   curves that do not add up. Gastin's own demand envelope was digitised too
#'   and agrees with the sum to 1.6-3.4 mL O2/kg/min on average -- that
#'   disagreement is the reading error, and it is reported by the test rather
#'   than being buried in whichever curve happened to be derived.
gastin_case <- function(case, dt = 0.25, vo2_rest = 3.5) {
  cs <- GASTIN_CASES[[case]]
  if (is.null(cs)) stop("Unknown Gastin case: ", case)
  t <- seq(0, cs$duration, by = dt)
  d <- data.frame(time = t, atp_pcr = cs$atp_pcr(t), glyco = cs$glyco(t),
                  aerobic = cs$aerobic(t))
  # Gastin's curves include resting metabolism; the model is above-rest. Only
  # the aerobic curve carries a resting component, so only it is adjusted -- the
  # demand then follows, since it is the sum.
  d$aerobic <- pmax(d$aerobic - vo2_rest, 0)
  d$demand  <- d$atp_pcr + d$glyco + d$aerobic
  d <- d[, c("time", "demand", "atp_pcr", "glyco", "aerobic")]
  attr(d, "case") <- cs
  attr(d, "vo2_rest") <- vo2_rest
  d
}

#' A case's VO2max on the model's ABOVE-REST scale
#'
#' The published VO2max is a gross measurement, so the model's aerobic ceiling
#' -- which is an above-rest flux -- has to have resting VO2 taken off it too,
#' or the ceiling would sit above the curve it is being compared with.
gastin_case_vo2max <- function(case, vo2_rest = 3.5) {
  cs <- GASTIN_CASES[[case]]
  if (is.null(cs)) stop("Unknown Gastin case: ", case)
  cs$VO2max - vo2_rest
}

#' O2 deficit implied by a case (mL O2/kg): the two anaerobic supplies integrated
gastin_case_deficit <- function(case, dt = 0.25) {
  d <- gastin_case(case, dt)
  sum((d$atp_pcr + d$glyco) / 60) * dt
}

GASTIN_CASE_NAMES <- c(
  "Fig. 5 — 90 s all-out"          = "fig5_allout",
  "Fig. 5 — 110% VO2max constant"  = "fig5_110",
  "Fig. 4 — sprint-trained"        = "fig4_sprint",
  "Fig. 4 — endurance-trained"     = "fig4_endurance"
)

# -----------------------------------------------------------------------------
# Configuration fitted jointly to the two Fig. 5 cases (one athlete, two
# protocols), by Nelder-Mead on the RMSE of the three supply curves. Overwritten
# by the fitting script; see the "Gastin cases" tab for what it reproduces.
# -----------------------------------------------------------------------------
# Joint RMSE over the three supply curves of both Fig. 5 cases: 4.41 mL/kg/min
# (from 10.67 at di Prampero's defaults). Notable: C_P stayed at 376 J/kg, the
# published alactic capacity, without being held there.
GASTIN_FIT <- list(ell1 = 0.1771, ell2 = 0.0273, gl_h = 0.5050, C_P = 376.0,
                   Amax = 87.84, gA = 0.3742, tauZ = 24.19, tauA = 11.51)

# -----------------------------------------------------------------------------
# FLOATING-TUBE VARIANT (separate model, separate tab).
#
# Tube A follows P's surface instead of sitting at a fixed height, so the head is
# the whole depth of lactic fluid standing above P and keeps growing as P drains.
# Glycolysis then switches on only as P falls -- the activation lag is emergent,
# so tau_A is 0 here by construction. tau_Z is kept: the aerobic tap still has a
# response time, held near 20 s.
#
# Fitted separately to the same Gastin case figures; gA_head_ref is unused
# because the floating tube normalises geometrically.
# -----------------------------------------------------------------------------
# Fitted to the two Fig. 5 cases: RMSE 4.12 mL/kg/min, against 4.41 for the
# fixed tube -- and with one FEWER free parameter, since tau_A is structural (0)
# and gA_head_ref is unused. tau_Z is PINNED at 20 s rather than fitted: the fit
# wanted 24.6 s, but 20 s is the stated ceiling and VO2max must still arrive
# inside 2 min.
GASTIN_FIT_FLOAT <- list(ell1 = 0.2661, ell2 = 0.0188, gl_h = 0.6571, C_P = 385.3,
                         Amax = 134.5, gA = 0.3742, tauZ = 20, tauA = 0,
                         float = TRUE)
