# Dynamic (Mader-style) metabolic model ---------------------------------------
#
# A minimal port of the dynamic 5-state ODE bioenergetic model implemented in the
# MuscleEdot package (J. Briand), itself an implementation of Mader's dynamic
# metabolic model. It integrates a measured metabolic-power demand into
# instantaneous aerobic (oxidative phosphorylation), glycolytic (lactic) and
# alactic (phosphocreatine) power. Only the pieces needed to simulate and
# partition a single sprint are ported here (no fatigue / MAP-test / dashboard
# code). Equations and kinetic constants are reproduced verbatim from MuscleEdot
# so the two implementations agree numerically (see the parity test).
#
# State vector u = (gp, vo2, lam, lab, vo2_mouth):
#   gp        composite high-energy phosphate pool (PCr + adenylates), mmol/kg
#   vo2       muscle oxygen uptake, mL O2 / kg muscle / s
#   lam       muscle lactate, mmol/L
#   lab       blood lactate, mmol/L
#   vo2_mouth mouth-level VO2 (respiratory lag), mL O2 / kg / min
#
# Units: metabolic power in W/kg, time in s, VO2max in mL/kg/min (systemic),
# VLamax in mmol/kg/s (systemic), concentrations in mmol/kg or mmol/L.
#
# @source Ported from the MuscleEdot package (metabolic_ODE_system.R,
#   oxphos_regulation.R, glycolysis_regulation.R, lactate_oxydation.R,
#   high_energy_phosphates.R), which implements Mader's dynamic metabolic model.
# @references Mader A (2003). Glycolysis and oxidative phosphorylation as a
#   function of cytosolic phosphorylation state and power output of the muscle
#   cell. European Journal of Applied Physiology, 88(4-5), 317-338.

# --- Systemic -> muscle unit conversions -------------------------------------

#' Convert systemic VO2max to muscle-specific VO2max
#'
#' `vo2max_systemic / m_active / 60`: scales to active muscle mass and converts
#' mL/kg/min to mL O2 / kg muscle / s.
#' @noRd
vo2max_to_muscle <- function(vo2max_systemic, m_active = 0.25) {
  vo2max_systemic / m_active / 60
}

#' Convert systemic VLamax to muscle-specific VLamax
#' @noRd
vlamax_to_muscle <- function(vlamax_systemic, m_active = 0.25) {
  vlamax_systemic / m_active
}

#' Lactate release ratio between the active and passive compartments
#' @noRd
vrel_ratio <- function(active_compartment = 0.25, passive_compartment = 0.20) {
  active_compartment / passive_compartment
}

#' VO2-to-ATP coefficient (1 / ATP constant)
#' @noRd
bvo2_coef <- function(atp_constant) {
  1 / atp_constant
}

#' Metabolic-power-to-ATP coefficient
#' @noRd
bpower_coef <- function(bvo2, eq_o2, m_active) {
  bvo2 / (eq_o2 * m_active)
}

#' Convert glycolytic rate (vLa) to power (W/kg): molar mass x lactate energy eq.
#' @noRd
kpla_coef <- function(molar_mass = 89, la_eq_e = 1.109) {
  molar_mass * la_eq_e
}

#' Alactic (phosphocreatine) power from PCr depletion over one step
#'
#' Power is produced only while PCr is depleting (`delta_pcr < 0`). Uses the P/O
#' ratio and the O2 energy equivalent to convert the mmol/kg drop to W/kg.
#' @noRd
alactic_power <- function(delta_pcr, dt, j_per_ml_o2 = 17.8,
                          ml_per_mmol_o2 = 22.4, pto_ratio = 5.2) {
  if (is.na(delta_pcr) || delta_pcr >= 0) {
    return(0)
  }
  energy_pcr <- -delta_pcr / pto_ratio * j_per_ml_o2 * ml_per_mmol_o2 # J/kg
  energy_pcr / dt # W/kg
}

# --- High-energy phosphate (Lohman) equilibrium ------------------------------

#' Intracellular muscle pH from phosphate and lactate status
#' @noRd
lohman_pHm <- function(pcr, vo2, lam, vo2max_muscle = 60, sc = 25) {
  pco2 <- 40 + 55 * vo2 / vo2max_muscle
  pi_conc <- sc - pcr
  7.85 + (0.8 * pi_conc - lam) / 54 - 0.55 * log10(pco2)
}

#' [AMP], [ADP], [ATP] from [PCr] and pH (creatine-/adenylate-kinase equilibria)
#' @noRd
lohman_phosphates_from_pcr <- function(pcr, pHm, sc = 25, sa = 7,
                                       m2 = 1.66, m3 = 0.95) {
  m1 <- m2 * 10^(9 - pHm)
  k <- (sc - pcr) / (m1 * pcr)
  adp <- k * sa / (1 + k + (k^2) * m3)
  atp <- adp / k
  amp <- k * m3 * adp
  list(amp = amp, adp = adp, atp = atp)
}

#' Recover [AMP], [ADP], [ATP], [PCr], pH from the composite pool GP
#'
#' Solves for PCr by minimising the self-consistency residual over `[0, sc]`.
#' @noRd
lohman_phosphates_from_gp <- function(gp, vo2, lam, vo2max_muscle = 60, sc = 25,
                                      sa = 7, m2 = 1.66, m3 = 0.95) {
  eps <- 1e-6
  big <- 1e12
  pcr_objective <- function(pcr) {
    if (!is.finite(pcr) || pcr <= 0 || pcr >= sc) return(big)
    pHm <- lohman_pHm(pcr, vo2, lam, vo2max_muscle = vo2max_muscle, sc = sc)
    if (!is.finite(pHm)) return(big)
    m1 <- m2 * 10^(9 - pHm)
    if (!is.finite(m1) || m1 <= 0) return(big)
    k <- (sc - pcr) / (m1 * pcr)
    if (!is.finite(k)) return(big)
    adp <- k * (gp - pcr)
    if (!is.finite(adp)) return(big)
    pcr_calc <- gp - sa + (1 + k * m3) * adp
    if (!is.finite(pcr_calc)) return(big)
    (pcr_calc - pcr)^2
  }
  opt <- stats::optimize(pcr_objective, interval = c(eps, sc - eps))
  pcr <- opt$minimum
  pHm <- lohman_pHm(pcr, vo2, lam, vo2max_muscle = vo2max_muscle, sc = sc)
  phos <- lohman_phosphates_from_pcr(pcr, pHm, sc = sc, sa = sa, m2 = m2, m3 = m3)
  list(amp = phos$amp, adp = phos$adp, atp = phos$atp, pcr = pcr, pHm = pHm)
}

#' Resting initial [AMP], [ADP], [ATP], pH from [PCr] and [La]b
#' @noRd
lohman_initial_concentrations <- function(pcr, lab, vo2max_muscle = 60,
                                          vlamax_muscle = 2.4, k2 = 0.111^3,
                                          k3 = 20.2, sc = 25, sa = 7,
                                          m2 = 1.66, m3 = 0.95) {
  pHm_objective <- function(pHm_vec) {
    pHm <- pHm_vec[1]
    m1 <- m2 * 10^(9 - pHm)
    k <- (sc - pcr) / (m1 * pcr)
    adp <- k * sa / (1 + k + (k^2) * m3)
    amp <- k * m3 * adp
    vlass <- glycolysis_rate(adp, amp, pHm, vlamax_muscle = vlamax_muscle,
                             k2 = k2, k3 = k3)
    lam <- lab + 1.35 / (1 + 2 * 1.35) * vlass
    pHm_calc <- lohman_pHm(pcr, vlass, lam, vo2max_muscle = vo2max_muscle, sc = sc)
    (pHm_calc - pHm)^2
  }
  opt <- stats::optim(par = 7, fn = pHm_objective, method = "L-BFGS-B",
                      lower = 6, upper = 7.5)
  pHm <- opt$par
  phos <- lohman_phosphates_from_pcr(pcr, pHm, sc = sc, sa = sa, m2 = m2, m3 = m3)
  list(amp = phos$amp, adp = phos$adp, atp = phos$atp, pHm = pHm)
}

# --- Metabolic rate laws -----------------------------------------------------

#' Steady-state oxidative phosphorylation rate (Hill in [ADP]^2)
#' @noRd
oxphos_rate <- function(adp, vo2max_muscle = 60, k1 = 0.035^2) {
  vo2max_muscle / (1 + k1 / (adp^2))
}

#' Steady-state glycolytic (lactate production) rate with pH inhibition
#' @noRd
glycolysis_rate <- function(adp, amp, pHm, vlamax_muscle = 2.4,
                            k2 = 0.111^3, k3 = 20.2) {
  ph_factor <- 1 / (1 + 10^(k3 - 3 * pHm))
  vlamax_muscle / (1 + k2 / (adp * amp)) * ph_factor
}

#' Oxidative lactate removal rate (O2-dependent, saturating in [La]^2)
#' @noRd
lactate_oxidation_rate <- function(vo2, lam, k4 = 2) {
  0.0202 * vo2 / (1 + k4 / (lam^2))
}

# --- ODE system and integration ----------------------------------------------

#' Default fixed parameters of the dynamic model (non-fatigue engine)
#'
#' Reproduces the constants hard-coded in MuscleEdot's `simulate_metabolism()` /
#' `analyze_solution()` (the engine used for the ECSS energy-partition figure).
#' @noRd
dynamic_model_pars <- function(vo2max, vlamax, sc, sa, m_active,
                               atp_constant = 4.3) {
  list(
    VO2MAX = vo2max, VLAMAX = vlamax, SC = sc, SA = sa,
    M2 = 1.66, M3 = 0.95,
    K1 = 0.035^2, K2 = 0.111^3, K3 = 20.2, K4 = 2,
    BVLA = 1.4, TVO2 = 10, T_mouth = 30,
    Active_compartment = 0.25, Passive_compartment = 0.20,
    M_active = m_active, ATP_constant = atp_constant,
    EQ_O2 = 17.8, KPVO2 = 17.8, vo2_rest = 0
  )
}

#' Right-hand side of the 5-state dynamic metabolic ODE
#' @noRd
dynamic_metabolic_ode <- function(t, u, pars) {
  gp <- u[1]; vo2 <- u[2]; lam <- u[3]; lab <- u[4]; vo2_mouth <- u[5]

  vo2max_m <- vo2max_to_muscle(pars$VO2MAX, pars$M_active)
  vlamax_m <- vlamax_to_muscle(pars$VLAMAX, pars$M_active)
  vrel <- vrel_ratio(pars$Active_compartment, pars$Passive_compartment)
  bvo2 <- bvo2_coef(pars$ATP_constant)
  # bpower uses the canonical ATP constant (4.3), as in MuscleEdot's engine
  bpower <- bpower_coef(bvo2_coef(4.3), pars$EQ_O2, pars$M_active)

  power_demand <- pars$power_fun(t)

  phos <- lohman_phosphates_from_gp(gp, vo2, lam, vo2max_muscle = vo2max_m,
                                    sc = pars$SC, sa = pars$SA,
                                    m2 = pars$M2, m3 = pars$M3)
  adp <- phos$adp; amp <- phos$amp; pHm <- phos$pHm

  vo2ss <- oxphos_rate(adp, vo2max_muscle = vo2max_m, k1 = pars$K1)
  vlass <- glycolysis_rate(adp, amp, pHm, vlamax_muscle = vlamax_m,
                           k2 = pars$K2, k3 = pars$K3)
  vlaox <- lactate_oxidation_rate(vo2, lam, k4 = pars$K4)
  beta <- 0.065 / (lab^1.4)

  dgp <- bvo2 * (vo2 - pars$vo2_rest) + pars$BVLA * vlass - bpower * power_demand
  dvo2 <- (vo2ss - vo2) / pars$TVO2
  dlam <- -beta * (lam - lab) + 1.35 * (vlass - vlaox * 2 / 3)
  dlab <- vrel * (beta * (lam - lab) - vlaox / 3)
  dvo2_mouth <- (vo2 - vo2_mouth / 60 / pars$M_active) / pars$T_mouth * 60 * pars$M_active

  list(c(dgp, dvo2, dlam, dlab, dvo2_mouth))
}

#' Integrate the dynamic metabolic ODE over a power-demand profile
#'
#' Returns a matrix with columns time, gp, vo2, lam, lab, vo2_mouth.
#' @noRd
run_dynamic_metabolism <- function(time, power_demand, vo2max, vlamax, sc, sa,
                                   m_active, atp_constant = 4.3, pcr_0 = 23,
                                   lab_0 = 1.1, vo2_rest_mouth = 0) {
  pars <- dynamic_model_pars(vo2max, vlamax, sc, sa, m_active, atp_constant)
  vo2max_m <- vo2max_to_muscle(pars$VO2MAX, pars$M_active)
  vlamax_m <- vlamax_to_muscle(pars$VLAMAX, pars$M_active)

  init <- lohman_initial_concentrations(
    pcr_0, lab_0, vo2max_muscle = vo2max_m, vlamax_muscle = vlamax_m,
    k2 = pars$K2, k3 = pars$K3, sc = pars$SC, sa = pars$SA,
    m2 = pars$M2, m3 = pars$M3)

  gp_0 <- pcr_0 + init$atp
  vo2_0 <- oxphos_rate(init$adp, vo2max_muscle = vo2max_m, k1 = pars$K1)
  vlass0 <- glycolysis_rate(init$adp, init$amp, init$pHm, vlamax_muscle = vlamax_m,
                            k2 = pars$K2, k3 = pars$K3)
  lam_0 <- lab_0 + 1.35 / (2 * 1.35 + 1) * vlass0

  u0 <- c(gp_0, vo2_0, lam_0, lab_0, vo2_rest_mouth)
  pars$power_fun <- stats::approxfun(x = time, y = power_demand, rule = 2)
  pars$vo2_rest <- vo2_0

  out <- deSolve::ode(y = u0, times = time, func = dynamic_metabolic_ode,
                      parms = pars, method = "rk4")
  out <- matrix(as.numeric(out), nrow = nrow(out), ncol = ncol(out))
  colnames(out) <- c("time", "gp", "vo2", "lam", "lab", "vo2_mouth")
  out
}

#' Partition an integrated solution into aerobic / lactic / alactic power (W/kg)
#'
#' Reproduces MuscleEdot's `analyze_solution()` power partition.
#' @noRd
partition_dynamic_solution <- function(sol, vo2max, vlamax, sc, sa, m_active,
                                       atp_constant = 4.3) {
  pars <- dynamic_model_pars(vo2max, vlamax, sc, sa, m_active, atp_constant)
  vo2max_m <- vo2max_to_muscle(pars$VO2MAX, pars$M_active)
  vlamax_m <- vlamax_to_muscle(pars$VLAMAX, pars$M_active)

  n <- nrow(sol)
  time <- sol[, "time"]
  gp <- sol[, "gp"]; vo2 <- sol[, "vo2"]; lam <- sol[, "lam"]
  vo2_rest <- min(vo2, na.rm = TRUE)

  vlass <- numeric(n); pcr <- numeric(n)
  for (i in seq_len(n)) {
    phos <- lohman_phosphates_from_gp(gp[i], vo2[i], lam[i], vo2max_muscle = vo2max_m,
                                      sc = pars$SC, sa = pars$SA,
                                      m2 = pars$M2, m3 = pars$M3)
    vlass[i] <- glycolysis_rate(phos$adp, phos$amp, phos$pHm,
                                vlamax_muscle = vlamax_m, k2 = pars$K2, k3 = pars$K3)
    pcr[i] <- phos$pcr
  }

  power_vo2 <- pars$KPVO2 * (vo2 - vo2_rest) * pars$M_active
  power_la <- kpla_coef() * vlass * pars$M_active

  delta_pcr <- c(pcr[-1], NA_real_) - pcr
  delta_t <- c(time[-1], NA_real_) - time
  power_alactic <- vapply(seq_len(n),
                          function(i) alactic_power(delta_pcr[i], delta_t[i]),
                          numeric(1)) * pars$M_active

  list(power_vo2 = power_vo2, power_la = power_la, power_alactic = power_alactic,
       power_total = power_vo2 + power_la + power_alactic)
}
