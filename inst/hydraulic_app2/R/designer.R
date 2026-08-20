# =============================================================================
# RESERVOIR DESIGNER
#
# The schematic is the control surface: you set each reservoir's HEIGHT and
# WIDTH with the +/- buttons drawn on it, and the energy each one holds follows
# from its AREA. One constant, K_AREA (J/kg per unit drawn area), converts the
# picture into physiology, so the drawing and the numbers can never disagree.
#
#   P    energy = K_AREA * w_P * 1                     (spans the full vessel)
#   Gl   energy = K_AREA * w_Gl * gl_h
#   eLa  energy = K_AREA * (w_eLa_bot + w_eLa_top)/2 * ela_h     (a wedge)
#   OS   energy = K_AREA * [ neck + tapered body ]  (see os_area())
#
# VO2max is an INDEPENDENT parameter, as in Morton: "the maximal flow through
# pipe R1 represents the maximal oxygen uptake" (Morton 2006, as restated by
# Weigend et al. 2022, arXiv:2207.14295). L1 is the height of the Z / Z1 tubes
# and sets only WHERE that maximum is reached -- the aerobic flow saturates once
# P has fallen to L1, because below the tube the discharge is free and the head
# stops growing:
#
#   Z' = Z'max * (1 - h_P) / (1 - L1),  capped at Z'max = VO2max * EQ_O2 / 60
#
# so the tube's conductance g_Z = Z'max / (1 - L1) is DERIVED, not fixed. An
# earlier version fixed g_Z and derived VO2max from L1, which made one control
# do two jobs. L2 is the floor of the lactic column and moves Gl up or down.
#
# The reference values are those of Briand, di Prampero, Osgnach, Thibault,
# Tremblay et al. 2025, Eur J Appl Physiol 125:3521-3541 (elite males), so every
# adjusted reservoir can be read against di Prampero's own figure.
# =============================================================================

EQ_O2_D <- 20.9
EQ_LA_D <- 3.0

# ---- di Prampero's reference reservoirs -------------------------------------
DIP_REF <- list(
  VO2max  = 60,      # mL/kg/min   (the app's reference athlete)
  P       = 376,     # J/kg        maximal anaerobic ALACTIC capacity
  Gl      = 1127,    # J/kg        glycolytic body
  eLa     = 188,     # J/kg        early lactate (3.0 mM x beta x EQ_O2)
  La      = 1315,    # J/kg        total lactic capacity (Gl + eLa)
  OS      = 4.3      # mL O2/kg    O2 stores
)

# Energy per unit drawn area. Chosen so the DEFAULT design below reproduces the
# reference reservoirs exactly, with P drawn 10 units wide.
K_AREA <- DIP_REF$P / 10

#' Default design: di Prampero's own geometry, drawn to scale
#'
#' Areas are proportional to energy on one common scale, so Gl (1127 J/kg) is
#' about 3x P (376 J/kg) in AREA -- the ratio you get from the two capacities.
design_defaults <- function() {
  ell1 <- 0.335
  list(
    vo2max      = DIP_REF$VO2max,   # independent, as Morton's R1 capacity
    ell1        = ell1,
    ell2        = 0,
    # P has a width at the base and at the top: equal = di Prampero's cylinder,
    # top < base = the conic P (Mader's non-linear activation), because a
    # narrower top means the level falls fast for little energy at the onset.
    w_P         = DIP_REF$P / K_AREA,                       # 10.0, at the BASE
    w_P_top     = DIP_REF$P / K_AREA,                       # equal -> cylinder
    gl_h        = ell1,
    # Gl has a width at its floor and one at its RIM. Equal = di Prampero's plain
    # rectangle; a wider rim puts most of the glycolytic energy HIGH in the
    # column, where it stays within reach of the head driving tube A, and leaves
    # a narrow foot -- the part his text calls unusable, blocked by acidification.
    w_Gl        = DIP_REF$Gl / K_AREA / ell1,
    w_Gl_top    = DIP_REF$Gl / K_AREA / ell1,               # equal -> rectangle
    ela_h       = 1 - ell1,
    w_eLa_bot   = 2 * (DIP_REF$eLa / K_AREA) / (1 - ell1) / (1 + 0.60) * 1,
    w_eLa_top   = 2 * (DIP_REF$eLa / K_AREA) / (1 - ell1) / (1 + 0.60) * 0.60,
    # OS is NOT full width at the top. Draining starts at the surface, and the
    # O2 dissociation curve means little O2 comes out per unit fall in PO2 while
    # the blood is still near-saturated: a narrow NECK. Below it the tank widens
    # into the body, where most of the store sits, tapering again toward L1.
    # Wall curvature, y = x^n with the origin at the wedge's base (eLa) and at
    # the shelf's outer corner (OS). n = 1 is a straight taper. These live in
    # the DESIGN, not just in the model, because area is energy here -- a curved
    # wall holds less than a straight one between the same endpoints.
    ela_pow     = 1,
    os_pow      = 1,
    # measure the OS curvature from L1 upward, so it bends with eLa
    os_from_base = FALSE,
    os_neck_h   = 0.30,                                     # neck height, as a fraction of the OS span
    w_OS_neck   = 0.9,                                      # neck (early efflux) width
    w_OS_body   = 8.6,                                      # widest part of the body
    w_OS_bot    = 0.9,                                      # width where it meets L1
    g_Z         = NA                                        # filled below
  )
}

#' Drawn area of the OS tank: a narrow neck on top of a tapered body
os_area <- function(d) {
  span   <- 1 - d$ell1
  h_neck <- d$os_neck_h * span
  h_body <- span - h_neck
  # The reservoir wall is w(v) = w_body - (w_body - w_bot) * v^(1/n) over the
  # depth v = 0..1, so its mean width is w_body - (w_body - w_bot) * n/(n + 1).
  # n = 1 gives the trapezoid (w_body + w_bot)/2, as before.
  k <- .pow_mean(d$os_pow)
  mean_w <- if (isTRUE(d$os_from_base)) d$w_OS_bot + (d$w_OS_body - d$w_OS_bot) * k
            else                        d$w_OS_body - (d$w_OS_body - d$w_OS_bot) * k
  d$w_OS_neck * h_neck + mean_w * h_body
}

# mean of u^(1/n) over u in 0..1
.pow_mean <- function(n) { n <- max(n, 1e-6); n / (n + 1) }

# Gl's rim width; older designs (and tests) predate it, and a rectangle is the
# right fallback -- it is what those designs meant.
.w_Gl_top <- function(d) if (is.null(d$w_Gl_top)) d$w_Gl else d$w_Gl_top

#' Energies implied by a design (J/kg, except OS in mL O2/kg)
design_capacities <- function(d) {
  Zmax <- d$vo2max * EQ_O2_D / 60
  list(
    VO2max = d$vo2max,
    Zmax   = Zmax,
    g_Z    = Zmax / (1 - d$ell1),        # derived: the tube's bore
    P      = K_AREA * (d$w_P + d$w_P_top) / 2 * 1,
    Gl     = K_AREA * (d$w_Gl + .w_Gl_top(d)) / 2 * d$gl_h,
    # eLa's wall is w(u) = w_bot + (w_top - w_bot) * u^(1/n), so its mean width
    # is w_bot + (w_top - w_bot) * n/(n + 1); n = 1 gives the old trapezoid.
    eLa    = K_AREA * (d$w_eLa_bot +
                       (d$w_eLa_top - d$w_eLa_bot) * .pow_mean(d$ela_pow)) * d$ela_h,
    OS     = K_AREA * os_area(d) / EQ_O2_D
  )
}

#' Turn a design into hydraulic-model parameters
design_to_params <- function(d, ...) {
  cap <- design_capacities(d)
  hyd_defaults(
    VO2max           = cap$VO2max,
    alactic_capacity = max(cap$P, 1),
    gl_ml_per_kg     = max(cap$Gl, 1) / EQ_O2_D,
    ela_mmol         = max(cap$eLa, 1) / (EQ_LA_D * EQ_O2_D),
    os_ml_per_kg     = max(cap$OS, 0.01),
    ell1             = d$ell1,
    ell2             = d$ell2,
    gl_h             = d$gl_h,
    ela_h            = d$ela_h,
    # Gl's shape: the model's profile runs w(u) = 1 + (gl_taper - 1) * u from
    # floor to rim, so gl_taper IS the rim/floor width ratio drawn on the figure.
    gl_taper         = min(max(.w_Gl_top(d) / max(d$w_Gl, 1e-6), 0.05), 8),
    # the OS controls are no longer cosmetic: the drawn tube, shelf and
    # reservoir widths are the ones the model integrates
    os_tube_w        = d$w_OS_neck / max(d$w_OS_body, 1e-6),
    os_tube_h        = d$os_neck_h,
    os_bot_w         = d$w_OS_bot / max(d$w_OS_body, 1e-6),
    p_taper          = min(max(d$w_P_top / max(d$w_P, 1e-6), 0.05), 4),
    ela_taper        = max(d$w_eLa_top / max(d$w_eLa_bot, 1e-6), 0.02),
    ela_pow          = d$ela_pow,
    os_pow           = d$os_pow,
    os_from_base     = isTRUE(d$os_from_base),
    ...
  )
}

#' Nudge one field of the design, keeping the geometry legal
design_nudge <- function(d, field, step) {
  d[[field]] <- d[[field]] + step
  lim <- list(vo2max = c(20, 100), ela_pow = c(0.25, 5), os_pow = c(0.25, 5),
              ell1 = c(0.05, 0.90), ell2 = c(0, 0.60),
              w_P = c(1, 60), w_P_top = c(0.5, 60),
              gl_h = c(0.05, 0.95), w_Gl = c(2, 400), w_Gl_top = c(1, 400),
              ela_h = c(0.05, 0.95), w_eLa_bot = c(0.5, 200),
              w_eLa_top = c(0.5, 200), w_OS_bot = c(0.2, 60),
              w_OS_neck = c(0.2, 60), w_OS_body = c(0.5, 120),
              os_neck_h = c(0.05, 0.85))
  if (!is.null(lim[[field]])) {
    d[[field]] <- min(max(d[[field]], lim[[field]][1]), lim[[field]][2])
  }
  # The lactic column must fit inside the vessel: L2 + Gl + eLa <= 1. Give way in
  # order -- first the reservoir the user is NOT adjusting, then the other one,
  # and if both are already at their floor, refuse the nudge rather than let the
  # column climb out of the vessel.
  give <- function(d, who, over) {
    take <- min(over, d[[who]] - 0.05)
    if (take > 0) d[[who]] <- d[[who]] - take
    d
  }
  over <- d$ell2 + d$gl_h + d$ela_h - 1
  if (over > 1e-12) {
    order_of_giving <- if (field %in% c("ell2", "gl_h")) c("ela_h", "gl_h") else c("gl_h", "ela_h")
    order_of_giving <- setdiff(order_of_giving, field)
    for (who in order_of_giving) {
      d <- give(d, who, over)
      over <- d$ell2 + d$gl_h + d$ela_h - 1
      if (over <= 1e-12) break
    }
    if (over > 1e-12) d[[field]] <- d[[field]] - over    # the nudge itself gives way
  }
  d
}

#' Comparison of a design against di Prampero's reference reservoirs
design_vs_reference <- function(d) {
  cap <- design_capacities(d)
  mk <- function(name, val, ref, unit, digits = 0) data.frame(
    quantity = name, value = val, reference = ref, unit = unit,
    pct = 100 * val / ref, digits = digits, stringsAsFactors = FALSE)
  rbind(
    mk("VO₂max", cap$VO2max, DIP_REF$VO2max, "mL·kg⁻¹·min⁻¹", 1),
    mk("P",  cap$P,   DIP_REF$P,   "J·kg⁻¹"),
    mk("Gl", cap$Gl,  DIP_REF$Gl,  "J·kg⁻¹"),
    mk("eLa", cap$eLa, DIP_REF$eLa, "J·kg⁻¹"),
    mk("La (Gl+eLa)", cap$Gl + cap$eLa, DIP_REF$La, "J·kg⁻¹"),
    mk("OS", cap$OS,  DIP_REF$OS,  "mL O₂·kg⁻¹", 1)
  )
}
