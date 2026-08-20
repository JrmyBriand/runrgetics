# =============================================================================
# di Prampero hydraulic model — SELF-CONTAINED core for the Shiny app.
#
# Base R only: no deSolve, no cli, no tibble. This lets the app run unchanged in
# the browser under shinylive/webR (WebAssembly), where compiled packages such as
# deSolve are not guaranteed to be available. The ODE is integrated with a
# fixed-step classic RK4 that matches deSolve::ode(method = "rk4") on the same
# time grid; `tests/testthat/test-hydraulic_app_core.R` asserts the two agree.
#
# The equations are documented in R/hydraulic_model.R and in
# vignettes/hydraulic_model_diprampero.qmd. Keep the two in step.
#
# TANKS  P (alactic, ATP+PCr) · Ox (aerobic, ~infinite) · OS (O2 stores)
#        Gl (glycolytic/lactic) · eLa (early lactate)
# TUBES  Z (Ox->P at L1) · Z1 (OS->P at L1) · A (Gl->P at L_A) · eLa->Gl top
# TAP    S sets the outflow U' (work intensity)
#
# THE TWO CONFIGURATIONS OF TUBE A (`gl_refill`):
#   FALSE "di Prampero"  strict one-way valve, A' >= 0; Gl can only drain.
#   TRUE  "Morton"       reversible; production falls as Gl empties and the flow
#                        reverses into Gl during recovery, capped at M_R.
# =============================================================================


# ---- small helpers ----------------------------------------------------------
hyd_clip01 <- function(x) pmin(pmax(x, 0), 1)
hyd_ramp   <- function(x, w) pmin(pmax(x / w, 0), 1)


#' Default parameters (mirrors hydraulic_model_defaults() in the package)
hyd_defaults <- function(VO2max           = 60,
                         lactic_power     = NULL,
                         a_lactic         = 2.15,
                         alactic_capacity = 376,
                         ell1             = 0.335,
                         gl_refill        = FALSE,
                         a_recovery       = 2.5,
                         gA_head_ref      = 0.052,
                         ela_taper        = 0.60,
                         ela_pow          = 1,
                         os_pow           = 1,
                         os_tube_w        = 0.105,
                         os_tube_h        = 0.30,
                         os_bot_w         = 0.105,
                         os_from_base     = FALSE,
                         gl_taper         = 4.1,
                         p_taper          = 1,
                         ell2             = 0,
                         gl_h             = NULL,
                         ela_h            = NULL,
                         os_ml_per_kg     = 4.3,
                         gl_ml_per_kg     = 53.9,
                         ela_mmol         = 3.0,
                         tau_refill_Gl    = Inf,
                         tau_Z            = 0,
                         tau_A            = 0,
                         tau_Z1           = 0,
                         z1_max_frac      = 1,
                         float_tubes      = FALSE,
                         tau_mouth        = 6,
                         delta_mouth      = 8,
                         tau_clear        = 1300,
                         V_ratio          = 0.30,
                         k_eff            = 1 / 120,
                         Sv_rest          = 0.75,
                         Sv_min           = 0.25,
                         P50              = 26,
                         hill_n           = 2.7,
                         PCr_rest         = 20,
                         La_rest          = 1.0,
                         vo2_rest         = 3.5,
                         EQ_O2            = 20.9,
                         EQ_La            = 3.0,
                         gate_w           = 0.02) {
  stopifnot(ell1 > 0, ell1 < 1, ela_taper > 0, ela_taper <= 1,
            p_taper > 0, p_taper <= 4, gl_taper > 0, gl_taper <= 8, gA_head_ref > 0)

  Zmax    <- VO2max * EQ_O2 / 60
  # A'max is an ABSOLUTE power (W/kg), not a ratio to Zmax -- see the package
  # copy for why. vLamax (mmol/L/s) = Amax / 62 (Briand et al. 2025).
  Amax    <- if (is.null(lactic_power)) a_lactic * Zmax else lactic_power
  a_lactic <- Amax / Zmax
  C_P     <- alactic_capacity
  tau_pcr <- C_P * (1 - ell1) / Zmax
  M_R     <- Amax / a_recovery
  Cap_OS  <- os_ml_per_kg * EQ_O2
  Cap_Gl  <- gl_ml_per_kg * EQ_O2
  Cap_eLa <- ela_mmol * EQ_La * EQ_O2
  Cap_LA  <- Cap_Gl + Cap_eLa            # the two are ONE connected column

  # --- geometry of the lactic column (Gl below L1, eLa wedge above it) --------
  # Gl is a plain rectangle from the floor up to L1; eLa is di Prampero's WEDGE
  # standing on the Gl rim, its cross-section tapering from w_b at L1 to
  # w_b * ela_taper at L0. Cross-sectional area is what converts stored energy
  # into fluid LEVEL, so this shape is what makes eLa behave differently from a
  # simple extra tank: a narrow tip means the level falls quickly for little
  # energy, which is exactly the fast, small "early lactate" transient.
  s_eLa <- 1 - ell1
  a_Gl  <- Cap_Gl / ell1                                  # constant Gl cross-section
  g_A   <- Amax / gA_head_ref                             # A-tube conductance

  # Shaped-tank profiles (see the SHAPED TANKS section). The lactic column is one
  # profile spanning 0..1: a rectangle up to the Gl rim, then di Prampero's wedge.
  # Gl may itself be shaped. gl_taper = 1 is di Prampero's plain rectangle;
  # gl_taper > 1 widens it toward the rim, so most of the glycolytic energy sits
  # HIGH in the column and stays reachable, while the narrow foot is the part his
  # text calls unusable (acidification blocking full glycogen use).
  # The lactic column may sit anywhere in the vessel: its floor is L2 (`ell2`),
  # the Gl body has height `gl_h` and the eLa wedge of height `ela_h` stands on
  # it. Defaults keep di Prampero's arrangement (floor at 0, Gl up to L1, eLa
  # from L1 to L0), so nothing changes unless the geometry is moved.
  if (is.null(gl_h))  gl_h  <- ell1 - ell2
  if (is.null(ela_h)) ela_h <- 1 - (ell2 + gl_h)
  z_bot <- ell2; z_mid <- ell2 + gl_h; z_top <- min(z_mid + ela_h, 1)
  prof_Gl  <- hyd_build_profile(z_bot, z_mid, function(u) 1 + (gl_taper - 1) * u, Cap_Gl)
  prof_eLa <- hyd_build_profile(z_mid, z_top,
                                function(u) hyd_shape_ela(u, ela_taper, ela_pow), Cap_eLa)
  prof_LA  <- list(
    h = c(prof_Gl$h, prof_eLa$h[-1]),
    w = c(prof_Gl$w, prof_eLa$w[-1]),
    V = c(prof_Gl$V, Cap_Gl + prof_eLa$V[-1]),
    h_lo = z_bot, h_hi = z_top, capacity = Cap_LA)
  prof_OS  <- hyd_build_profile(ell1, 1, function(u) hyd_shape_os(u, os_pow, os_tube_w, os_tube_h,
                                                  os_bot_w, os_from_base), Cap_OS)
  # P's own shape. p_taper = 1 is di Prampero's plain cylinder; p_taper < 1 is the
  # CONIC P (base wider than top). A cone makes the level--content relation
  # NON-LINEAR: near the top the tank is narrow, so a little phosphagen energy
  # drops the level a long way, and the level is what drives Z' and A'. That is
  # the non-linear coupling between phosphagen state and the activation of the
  # oxidative and glycolytic pathways (cf. Mader 2003, Eur J Appl Physiol
  # 88:317-338, doi 10.1007/s00421-002-0676-3), and it lets the level fall
  # steeply while only a modest amount of ALACTIC energy is actually spent.
  prof_P <- hyd_build_profile(0, 1, function(u) 1 + (p_taper - 1) * u, C_P)

  list(
    tau_Z = tau_Z, tau_A = tau_A, tau_Z1 = tau_Z1, float_tubes = float_tubes,
    z1_max_frac = z1_max_frac,
    gl_span = max(z_top - z_bot, 1e-6),
    VO2max = VO2max, a_lactic = a_lactic, lactic_power = Amax,
    vlamax = Amax / 62, alactic_capacity = alactic_capacity,
    ell1 = ell1, ela_taper = ela_taper, ela_pow = ela_pow, os_pow = os_pow, os_tube_w = os_tube_w,
    os_tube_h = os_tube_h, os_bot_w = os_bot_w, os_from_base = os_from_base, gA_head_ref = gA_head_ref, g_A = g_A,
    gl_refill = gl_refill, a_recovery = a_recovery, M_R = M_R,
    Zmax = Zmax, Amax = Amax, C_P = C_P, tau_pcr = tau_pcr,
    Cap_OS = Cap_OS, Cap_Gl = Cap_Gl, Cap_eLa = Cap_eLa, Cap_LA = Cap_LA,
    s_eLa = s_eLa, a_Gl = a_Gl, ell2 = ell2, gl_h = gl_h, ela_h = ela_h,
    z_bot = z_bot, z_mid = z_mid, z_top = z_top,
    p_taper = p_taper, gl_taper = gl_taper, prof_P = prof_P,
    prof_LA = prof_LA, prof_OS = prof_OS, prof_Gl = prof_Gl, prof_eLa = prof_eLa,
    tau_refill_Gl = tau_refill_Gl, k_refill_Gl = 1 / tau_refill_Gl,
    tau_mouth = tau_mouth, delta_mouth = delta_mouth,
    tau_clear = tau_clear, k_clear = 1 / tau_clear,
    V_ratio = V_ratio, k_eff = k_eff,
    Sv_rest = Sv_rest, Sv_min = Sv_min, P50 = P50, hill_n = hill_n,
    PCr_rest = PCr_rest, La_rest = La_rest, vo2_rest = vo2_rest,
    EQ_O2 = EQ_O2, EQ_La = EQ_La, k_la = 1 / (EQ_O2 * EQ_La),
    os_ml_per_kg = os_ml_per_kg, gl_ml_per_kg = gl_ml_per_kg, ela_mmol = ela_mmol,
    gate_w = gate_w
  )
}


# =============================================================================
# SHAPED TANKS
#
# Every finite reservoir is defined by a WIDTH PROFILE w(h) — its cross-sectional
# area as a function of fluid level. The profile IS the physics: area converts
# stored energy into level, and level is what drives the tubes. The schematic
# draws each tank from the same profile, so the picture and the equations are one
# object rather than two descriptions that can drift apart.
#
# Both profiles are read off di Prampero's Fig. I-6.1:
#   OS  — a narrow neck at the top opening into a bulge that tapers to a point at
#         L1. That is the SLOPE of the venous O2 dissociation curve: little O2 is
#         released per unit fall in PO2 on the flat upper plateau, a great deal on
#         the steep middle, and little again once nearly desaturated.
#   eLa — a wedge standing on the Gl rim, widest at its base and tapering upward,
#         so its level falls quickly for little energy: the fast, small early
#         lactate transient rather than the behaviour of a second ordinary tank.
# =============================================================================

#' Width profile of OS (u = 0 at L1, u = 1 at L0)
#' Width profile of the O2 store
#'
#' Three pieces, as sketched, with u = 0 at L1 and u = 1 at L0:
#'
#'   TUBE            the top `tube_h` of the span, a narrow neck of width
#'                   `tube_w`. Little O2 leaves per unit fall in PO2 while the
#'                   blood is near-saturated, so the store starts thin.
#'   FLAT SECTION    where the tube meets the reservoir the width steps out to
#'                   1 (the body width). This is the shelf in the drawing.
#'   MAIN RESERVOIR  everything below. Put the origin at the OUTER corner of the
#'                   shelf, x measured inward and y downward: the wall is
#'                   y = x^pow, so the wall moves in as depth^(1/pow) and the
#'                   width runs from 1 at the shelf to `bot_w` where it meets L1.
#'
#'     w(v) = 1 - (1 - bot_w) * v^(1/pow),   v = depth below the shelf, 0..1
#'
#'   pow = 1   straight taper from shelf to floor
#'   pow > 1   narrows FAST just under the shelf, then runs nearly parallel
#'   pow < 1   holds its width down the reservoir, then closes abruptly
#'
#' Widths are relative to the body; hyd_build_profile() renormalises to the
#' capacity, so these set SHAPE only.
hyd_shape_os <- function(u, pow = 1, tube_w = 0.105, tube_h = 0.30,
                         bot_w = 0.105, from_base = FALSE) {
  u      <- pmin(pmax(u, 0), 1)
  tube_h <- min(max(tube_h, 1e-3), 0.95)
  u_shelf <- 1 - tube_h
  v      <- pmin(pmax((u_shelf - u) / u_shelf, 0), 1)      # depth below the shelf
  # `from_base` measures the curvature from L1 UPWARD instead of from the shelf
  # downward, so the OS wall becomes the same function of height-above-its-own-
  # floor as the eLa wedge -- the two walls then bend together rather than in
  # opposite senses. At pow = 1 both readings give the same straight taper.
  ub     <- pmin(pmax(u / u_shelf, 0), 1)                  # height above L1
  body   <- if (isTRUE(from_base)) bot_w + (1 - bot_w) * ub^(1 / max(pow, 1e-6))
            else                   1 - (1 - bot_w) * v^(1 / max(pow, 1e-6))
  pmax(ifelse(u >= u_shelf, tube_w, body), 1e-3)
}

#' Width profile of the eLa wedge
#'
#' Put the origin at the BASE of the wedge (the Gl rim), x horizontal and y
#' vertical, as in the sketch. The wall then follows
#'
#'   y = x^pow        equivalently   x = y^(1/pow)
#'
#' so the width, which is what x measures, interpolates between its two ends as
#'
#'   w(u) = 1 + (taper - 1) * u^(1/pow)
#'
#' with u the height as a fraction of the wedge. pow = 1 is the straight taper.
#' pow > 1 (e.g. the sketch's y = x^2) makes the width change fast near the BASE
#' and flatten toward the top; pow < 1 does the reverse. Both ends stay pinned
#' for any pow, since 0^k = 0 and 1^k = 1.
#'
#' The cross-section is what converts stored energy into LEVEL, so this sets how
#' quickly the lactic head falls as eLa drains.
hyd_shape_ela <- function(u, taper, pow = 1) {
  u <- pmin(pmax(u, 0), 1)
  pmax(1 + (taper - 1) * u^(1 / max(pow, 1e-6)), 1e-3)
}

#' Build a tank profile: levels, widths and cumulative volume
#'
#' @param h_lo,h_hi Level span of the tank.
#' @param w_fun Width as a function of normalised height within that span.
#' @param capacity Energy the tank holds when full (J/kg); the profile is scaled
#'   so that the integral of the width over the span equals it.
#' @param n Grid resolution.
#' @return `list(h, w, V, ...)`, `V` being cumulative volume measured from `h_lo`.
hyd_build_profile <- function(h_lo, h_hi, w_fun, capacity, n = 161) {
  h  <- seq(h_lo, h_hi, length.out = n)
  u  <- if (h_hi > h_lo) (h - h_lo) / (h_hi - h_lo) else rep(0, n)
  w  <- pmax(w_fun(u), 1e-9)
  dV <- c(0, 0.5 * (w[-1] + w[-n]) * diff(h))          # trapezoidal
  V  <- cumsum(dV)
  sc <- if (V[n] > 0) capacity / V[n] else 1
  list(h = h, w = w * sc, w_rel = w / max(w), V = V * sc,
       h_lo = h_lo, h_hi = h_hi, capacity = capacity)
}

#' Fluid level holding energy `Q`, given a tank profile
hyd_level_from_Q <- function(Q, prof) {
  stats::approx(x = prof$V, y = prof$h,
                xout = pmin(pmax(Q, 0), prof$capacity), rule = 2)$y
}

#' Fluid level of the lactic column (Gl body + eLa wedge)
#'
#' Because Gl and eLa form ONE column, draining always removes the topmost fluid
#' — eLa empties before Gl is touched — and filling always fills from the bottom
#' — Gl must be full before eLa takes anything. di Prampero's stacked
#' arrangement thus falls out of the geometry rather than from extra rules.
hyd_lactic_level <- function(Q, p) hyd_level_from_Q(Q, p$prof_LA)


#' Instantaneous algebraic flows (vector-safe)
hyd_algebra <- function(h_P, Q_OS, Q_LA, p, Z_act = NULL, A_act = NULL,
                         Z1_act = NULL) {
  hP <- hyd_clip01(h_P)
  gZ <- p$Zmax / (1 - p$ell1)

  # Aerobic Z' (Ox -> P): rises with P depletion, caps at Zmax (Morton).
  # Zss is the tube law's STEADY-STATE target. When the Ox tap is given a finite
  # response time (tau_Z > 0) the flow actually delivered lags it, exactly as in
  # Mader's model, where VO2ss is a function of [ADP] and the realised VO2
  # follows with dVO2/dt = (VO2ss - VO2)/TVO2. `Z_act` carries that lagged value;
  # with tau_Z = 0 the tap is instantaneous and Zp == Zss (di Prampero's tube).
  Zss <- p$Zmax * hyd_clip01((1 - hP) / (1 - p$ell1))
  Zp  <- if (is.null(Z_act)) Zss else pmin(pmax(Z_act, 0), p$Zmax)

  # O2 stores Z1' (OS -> P): same conductance as Z, driven by (h_OS - h_P);
  # bidirectional; discharge capped so Zp + Z1p never exceeds Zmax.
  sOS    <- hyd_clip01(Q_OS / p$Cap_OS)
  h_OS   <- hyd_level_from_Q(Q_OS, p$prof_OS)   # shape -> level (see SHAPED TANKS)
  # Z1 is at L1, the SAME level as Z and with the same resistance (chapter I-6).
  # Its head is h_OS - max(h_P, L1), which reads two equivalent ways: a tube
  # welded at L1 whose outlet is SUBMERGED while P stands above it (head to P's
  # surface) and discharging freely once P drops below it (head stops growing);
  # or an intake that rides on P's surface and bottoms out at L1, the store's own
  # floor -- the same relation A has to the column's floor at L2. Identical
  # arithmetic; the difference is only what the schematic draws, and it draws the
  # second, so the picture shows the head the model is using. Without the clamp
  # the head went on growing as P drained, ie the tube behaved as if at the
  # floor. It was masked at tau_Z = 0 by the Z + Z1 <= Z'max ceiling (with Z
  # already maximal there is nothing left for Z1), but at tau_Z = 10 s the O2
  # store was delivering up to 11.4 W/kg from below L1.
  Z1_raw <- gZ * (h_OS - pmax(hP, p$ell1))
  # Two separate ceilings on the store's flow. The JOINT one, Z + Z1 <= Z'max,
  # is the chapter's: Z1 has the same resistance as Z and no limit of its own.
  # `Z1_own` adds one, as a fraction of Z'max, on the reasoning that the cascade
  # emptying the venous store (dissociation, diffusion, convection) is not the
  # cascade that fixes maximal O2 uptake, so there is no reason the two should
  # share a ceiling. 1 is the default and reproduces the chapter exactly.
  Z1_own <- (if (is.null(p$z1_max_frac)) 1 else p$z1_max_frac) * p$Zmax
  Z1_cap <- pmin(pmax(p$Zmax - Zp, 0), Z1_own)
  # Arithmetic blend rather than ifelse(): identical result, and materially faster
  # in the scalar RK4 inner loop (4 calls per step).
  fwd    <- as.numeric(Z1_raw >= 0)
  Z1ss   <- fwd * pmin(Z1_raw, Z1_cap) * hyd_ramp(Q_OS, p$gate_w * p$Cap_OS) +
            (1 - fwd) * Z1_raw * hyd_ramp(p$Cap_OS - Q_OS, p$gate_w * p$Cap_OS)

  # The O2 store has its OWN response time, tau_Z1, independent of the Ox tap's.
  # There is no reason the two should share a value: tau_Z is a mitochondrial
  # lag, tau_Z1 would be transport. Physically the store has no enzymatic
  # activation to delay -- its discharge is set by the dissociation curve and by
  # diffusion, both fast -- so 0 is the default and this is here to be tested,
  # not assumed. The ceiling stays HARD on the delivered flow: a lagging Z1 must
  # still not push Z + Z1 above Zmax.
  Z1p <- if (is.null(Z1_act)) Z1ss else {
    z  <- pmin(Z1_act, Z1_cap)
    fw <- as.numeric(z >= 0)
    fw * z * hyd_ramp(Q_OS, p$gate_w * p$Cap_OS) +
      (1 - fw) * z * hyd_ramp(p$Cap_OS - Q_OS, p$gate_w * p$Cap_OS)
  }

  # Lactic flow through tube A, driven by the HEAD between the lactic column and
  # P — no extra thresholds. The A tube sits at the bottom, as di Prampero draws
  # it, so flow begins the moment P falls below the column. The column starts
  # full at L0, so a little flow appears from the very onset of exercise (early
  # lactate) and only reaches the Gl body once P has fallen below L1.
  h_LA <- hyd_lactic_level(Q_LA, p)
  # Once P's surface falls BELOW the lactic column's floor (L2), tube A is
  # discharging into air: lowering P further adds no head, and the driving
  # pressure is just the fluid depth left in the column. Morton handles this as
  # a separate branch, scaling the flow by the liquid remaining in the tank
  # (Weigend et al. 2021, arXiv:2104.07903, eq. 6, second case); clamping h_P at
  # the floor is the same thing. Without it the head goes on growing as P drains
  # and A' is overstated by up to 200% when L2 > 0. (No effect at L2 = 0, where
  # the column's floor is the vessel's floor.)
  head <- h_LA - pmax(hP, p$ell2)
  if (isTRUE(p$float_tubes)) {
    # Tube A floats on P's surface: the head is the depth of lactic fluid
    # standing above P, normalised by the column's own height (Morton's g_max).
    # `head` is already clamped at L2, and it has to be: below the column's own
    # floor there is no lactic fluid, only air, so the depth standing above P
    # stops growing there. Using (h_LA - hP) let the head go on growing as P
    # drained past L2, and drew the tube below the column it draws from. No
    # effect at the default L2 = 0, where pmax(hP, 0) is hP.
    Ass <- p$Amax * hyd_clip01(head / p$gl_span) *
             hyd_ramp(Q_LA, p$gate_w * p$Cap_LA)
  } else if (isTRUE(p$gl_refill)) {
    # Morton lineage: reversible, refill capped at the (smaller) recovery rate.
    raw <- p$g_A * head
    fw  <- as.numeric(raw >= 0)
    Ass <- fw * pmin(raw, p$Amax) * hyd_ramp(Q_LA, p$gate_w * p$Cap_LA) +
           (1 - fw) * pmax(raw, -p$M_R) * hyd_ramp(p$Cap_LA - Q_LA, p$gate_w * p$Cap_LA)
  } else {
    # di Prampero: strict one-way valve — the column can only drain.
    Ass <- pmin(p$g_A * pmax(head, 0), p$Amax) * hyd_ramp(Q_LA, p$gate_w * p$Cap_LA)
  }
  # Glycolysis does not switch on instantly: PFK activation takes seconds,
  # so the delivered flow lags the head-determined target with time constant
  # tau_A, exactly as the Ox tap lags with tau_Z. tau_A = 0 recovers Mader's
  # and di Prampero's instantaneous activation.
  Ap <- if (is.null(A_act)) Ass else
          pmin(pmax(A_act, -p$M_R), p$Amax) * hyd_ramp(Q_LA, p$gate_w * p$Cap_LA)

  # Which part of the column is currently supplying the flow? Above the Gl rim
  # it is the eLa wedge (early lactate); below it, the glycolytic body.
  from_eLa <- as.numeric(Q_LA > p$Cap_Gl)
  eLap <- from_eLa * pmax(Ap, 0)          # reported as early lactate
  Agl  <- (1 - from_eLa) * pmax(Ap, 0)    # reported as glycolytic

  list(Zp = Zp, Zss = Zss, Z1p = Z1p, Z1ss = Z1ss, Ap = Ap, Ass = Ass, eLap = eLap, Agl = Agl,
       h_OS = h_OS, h_LA = h_LA, sOS = sOS,
       lvl_Gl  = hyd_clip01(pmin(Q_LA, p$Cap_Gl) / p$Cap_Gl),
       lvl_eLa = hyd_clip01(pmax(Q_LA - p$Cap_Gl, 0) / p$Cap_eLa))
}


#' ODE right-hand side (6 states)
#'
#' Gl and eLa are ONE state (`Q_LA`, the connected lactic column), which is what
#' enforces "eLa empties first, Gl fills first" without any extra bookkeeping.
hyd_rhs <- function(t, y, p, power_fun) {
  # The state is the ENERGY stored in P, not its level: with a shaped (conic) P
  # the two are no longer proportional, and it is the energy that is conserved.
  Q_P <- y[1]; Q_OS <- y[2]; Q_LA <- y[3]
  VO2m <- y[4]; La_m <- y[5]; La_b <- y[6]; Z_act <- y[7]; A_act <- y[8]; Z1_act <- y[9]
  h_P <- hyd_level_from_Q(Q_P, p$prof_P)

  U <- power_fun(t)
  a <- hyd_algebra(h_P, Q_OS, Q_LA, p,
                   Z_act = if (p$tau_Z > 0) Z_act else NULL,
                   A_act = if (p$tau_A > 0) A_act else NULL,
                   Z1_act = if (p$tau_Z1 > 0) Z1_act else NULL)

  # Ap already carries the whole lactic flow (eLa region + Gl body).
  dQ_P <- a$Zp + a$Z1p + a$Ap - U
  if (Q_P <= 0 && dQ_P < 0) dQ_P <- 0
  if (Q_P >= p$C_P && dQ_P > 0) dQ_P <- 0

  dQ_OS <- -a$Z1p
  dQ_LA <- -a$Ap + p$k_refill_Gl * (p$Cap_LA - Q_LA)   # slow glycogen resynthesis (off by default)

  dVO2m <- (a$Zp - VO2m) / p$tau_mouth
  dLa_m <- p$k_la * max(a$Ap, 0) / p$V_ratio - p$k_eff * (La_m - La_b)
  dLa_b <- p$k_eff * p$V_ratio * (La_m - La_b) - p$k_clear * La_b

  # The tap moves toward its target with time constant tau_Z (0 = instantaneous)
  dZ_act <- if (p$tau_Z > 0) (a$Zss - Z_act) / p$tau_Z else 0
  dA_act <- if (p$tau_A > 0) (a$Ass - A_act) / p$tau_A else 0
  dZ1_act <- if (p$tau_Z1 > 0) (a$Z1ss - Z1_act) / p$tau_Z1 else 0

  c(dQ_P, dQ_OS, dQ_LA, dVO2m, dLa_m, dLa_b, dZ_act, dA_act, dZ1_act)
}


#' Fixed-step classic RK4 over the supplied time grid
hyd_rk4 <- function(y0, times, p, power_fun) {
  n   <- length(times)
  out <- matrix(NA_real_, nrow = n, ncol = length(y0))
  out[1, ] <- y0
  y <- y0
  for (i in seq_len(n - 1)) {
    t <- times[i]; h <- times[i + 1] - times[i]
    k1 <- hyd_rhs(t,         y,               p, power_fun)
    k2 <- hyd_rhs(t + h / 2, y + h / 2 * k1,  p, power_fun)
    k3 <- hyd_rhs(t + h / 2, y + h / 2 * k2,  p, power_fun)
    k4 <- hyd_rhs(t + h,     y + h * k3,      p, power_fun)
    y  <- y + (h / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
    out[i + 1, ] <- y
  }
  out
}


#' Simulate, then attach the algebraic flows and the measurable read-outs
#'
#' @param times Output/integration times (s, strictly increasing).
#' @param power O2-equivalent demand U' (W/kg body), same length as `times`.
#' @param p Parameter list from [hyd_defaults()].
#' @return A data.frame with levels, flows and read-outs (see the app).
hyd_simulate <- function(times, power, p) {
  stopifnot(length(times) == length(power), length(times) >= 2)
  power_fun <- stats::approxfun(times, power, rule = 2)

  # 7th state: the flow actually delivered by the Ox tap (see tau_Z)
  y0 <- c(p$C_P, p$Cap_OS, p$Cap_LA, 0, 0, 0, 0, 0, 0)
  m  <- hyd_rk4(y0, times, p, power_fun)

  Q_P <- pmin(pmax(m[, 1], 0), p$C_P)
  d <- data.frame(
    time  = times,
    Q_P   = Q_P,
    h_P   = hyd_level_from_Q(Q_P, p$prof_P),
    Q_OS  = pmin(pmax(m[, 2], 0), p$Cap_OS),
    Q_LA  = pmin(pmax(m[, 3], 0), p$Cap_LA),
    VO2_mouth = m[, 4],
    La_m  = pmax(m[, 5], 0),
    La_b  = pmax(m[, 6], 0),
    U     = power
  )

  a <- hyd_algebra(d$h_P, d$Q_OS, d$Q_LA, p,
                   Z_act = if (p$tau_Z > 0) m[, 7] else NULL,
                   A_act = if (p$tau_A > 0) m[, 8] else NULL,
                   Z1_act = if (p$tau_Z1 > 0) m[, 9] else NULL)
  d$Zp <- a$Zp; d$Zss <- a$Zss; d$Ass <- a$Ass; d$Z1ss <- a$Z1ss; d$Z1p <- a$Z1p; d$Ap <- a$Ap
  d$eLap <- a$eLap; d$Agl <- a$Agl
  d$h_OS <- a$h_OS; d$h_LA <- a$h_LA

  # Alactic power = demand not met by the inflows (PCr splitting when > 0,
  # resynthesis when < 0). A deficit only exists once P is empty.
  inflow      <- a$Zp + a$Z1p + a$Ap
  d$alactic_W <- d$U - inflow
  floored     <- d$h_P <= 1e-3
  d$deficit_W <- ifelse(floored, pmax(d$U - inflow, 0), 0)

  # Tank fill fractions for the schematic (1 = full/rest).
  d$lvl_P   <- d$h_P
  d$lvl_Ox  <- 1
  d$lvl_OS  <- hyd_clip01(d$Q_OS / p$Cap_OS)
  d$lvl_Gl  <- a$lvl_Gl     # full until the eLa wedge above it has emptied
  d$lvl_eLa <- a$lvl_eLa    # and only refills once Gl is full again

  # Measurables.
  d$pcr <- p$PCr_rest * d$h_P
  d$vo2_muscle_W <- a$Zp + a$Z1p
  d$vo2_muscle_mlkgmin <- d$vo2_muscle_W / p$EQ_O2 * 60 + p$vo2_rest
  mouth_delayed <- stats::approx(d$time, d$VO2_mouth,
                                 xout = d$time - p$delta_mouth, rule = 2)$y
  d$vo2_mouth_W <- mouth_delayed
  d$vo2_mouth_mlkgmin <- mouth_delayed / p$EQ_O2 * 60 + p$vo2_rest
  d$La_b_display <- d$La_b + p$La_rest
  d$La_m_display <- d$La_m + p$La_rest

  # Cumulative pathway energies (J/kg).
  trap <- function(x, t) {
    n <- length(t); if (n < 2) return(rep(0, n))
    c(0, cumsum(0.5 * (x[-1] + x[-n]) * diff(t)))
  }
  d$cum_aerobic    <- trap(a$Zp, d$time)
  d$cum_store      <- trap(pmax(a$Z1p, 0), d$time)
  d$cum_lactic     <- trap(pmax(a$Ap, 0), d$time)   # eLa wedge + Gl body
  d$cum_eLa        <- trap(a$eLap, d$time)          # the early-lactate share of it
  d$cum_gl_refill  <- trap(pmax(-a$Ap, 0), d$time)
  d$cum_alactic    <- trap(pmax(ifelse(floored, 0, d$alactic_W), 0), d$time)
  d$cum_deficit    <- trap(d$deficit_W, d$time)
  d$o2_deficit     <- trap(pmax(d$U - a$Zp, 0), d$time)
  d$o2_debt        <- trap(pmax(a$Zp - d$U, 0), d$time)
  d
}


#' Build a square-wave exercise protocol
#'
#' @param type "continuous" or "intermittent".
#' @param intensity Work intensity as a fraction of VO2max (e.g. 1.05 = 105%).
#' @param duration Work duration (s) for `type = "continuous"`.
#' @param work,rest Bout and recovery durations (s) for `type = "intermittent"`.
#' @param n_reps Number of work bouts.
#' @param rest_intensity Intensity during the between-bout recovery (fraction).
#' @param recovery Trailing recovery at rest (s).
#' @param warmup Rest baseline before the first bout (s).
#' @param p Parameter list (supplies `Zmax`).
#' @param dt Time step (s). `NULL` (default) picks it automatically so the run
#'   stays around `max_steps` RK4 steps — bounded cost whatever the protocol
#'   length, which matters in the browser (webR) where R is slower.
#' @param max_steps Target number of integration steps when `dt = NULL`.
#' @return list(time, power, marks) — `marks` are bout start/end times (s).
hyd_protocol <- function(type = c("continuous", "intermittent"),
                         intensity = 0.8, duration = 180,
                         work = 30, rest = 30, n_reps = 10,
                         rest_intensity = 0, recovery = 300,
                         warmup = 10, p, dt = NULL, max_steps = 1600) {
  type <- match.arg(type)
  if (is.null(dt)) {
    segs  <- if (type == "continuous") c(warmup, duration, recovery)
             else c(warmup, work, rest, recovery)
    segs  <- segs[segs > 0]
    total <- warmup + recovery +
      if (type == "continuous") duration else n_reps * (work + rest)
    # Aim for ~max_steps, but never step so coarsely that the SHORTEST square-wave
    # segment is under-resolved (>= ~15 steps per segment); 2 s hard ceiling keeps
    # the fastest time constant (tau_mouth ~ 6 s) comfortably resolved for RK4.
    dt_max <- min(2, if (length(segs)) min(segs) / 15 else 2)
    dt <- min(max(total / max_steps, 0.05), dt_max)
  }
  Z <- p$Zmax
  seg_lv <- numeric(0); seg_du <- numeric(0)
  if (warmup > 0) { seg_lv <- c(seg_lv, 0); seg_du <- c(seg_du, warmup) }
  if (type == "continuous") {
    seg_lv <- c(seg_lv, intensity * Z); seg_du <- c(seg_du, duration)
  } else {
    for (i in seq_len(n_reps)) {
      seg_lv <- c(seg_lv, intensity * Z, rest_intensity * Z)
      seg_du <- c(seg_du, work, rest)
    }
  }
  if (recovery > 0) { seg_lv <- c(seg_lv, 0); seg_du <- c(seg_du, recovery) }

  tt <- 0; uu <- 0; t0 <- 0; marks <- numeric(0)
  for (i in seq_along(seg_lv)) {
    ti <- seq(t0 + dt, t0 + seg_du[i], by = dt)
    if (length(ti)) {
      tt <- c(tt, ti); uu <- c(uu, rep(seg_lv[i], length(ti)))
    }
    if (seg_lv[i] > 0) marks <- c(marks, t0, t0 + seg_du[i])
    t0 <- t0 + seg_du[i]
  }
  list(time = tt, power = uu, marks = marks, t_end = t0)
}
