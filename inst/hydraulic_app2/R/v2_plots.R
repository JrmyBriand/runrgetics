# =============================================================================
# PLOTS FOR THE SIMULATOR (v2)
#
# ONE definition of the three pathway bands, used by the pathway plot, by the
# cumulative-energy plot and by the head-line numbers. That is deliberate: in v1
# the value boxes integrated `Zp` while the plot drew `Zp + Z1p`, so the numbers
# and the picture were quietly answering different questions. Here the head-line
# energies ARE the areas of the bands you can see.
# =============================================================================

HY2_PAL <- c("Aerobic"  = "#0072B2",     # Z' + Z1'  (Ox tap + O2 store)
             "Lactic"   = "#D55E00",     # A'        (Gl body + eLa wedge)
             "Alactic"  = "#009E73")     # demand not met by the inflows (PCr)

# The legend carries the symbol as well as the name: which tube each band is the
# flow through is the whole point of reading the figure against the schematic.
HY2_LEGEND <- c("Aerobic" = "aerobic  Z′+Z₁′",
                "Lactic"  = "lactic  A′",
                "Alactic" = "alactic  PCr↓")

# demand the reservoirs could not meet. Not a pathway: a shortfall.
HY2_UNMET  <- "#b6bec7"
HY2_UNMET_LAB <- "unmet demand"

HY2_INK  <- "#1f2933"
HY2_MUTE <- "#7b8794"
HY2_GRID <- "#eceff3"

theme_hy2 <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = HY2_GRID, linewidth = 0.45),
      legend.position  = "top",
      legend.title     = ggplot2::element_blank(),
      legend.margin    = ggplot2::margin(0, 0, 3, 0),
      legend.key.height = ggplot2::unit(0.95, "lines"),
      legend.text      = ggplot2::element_text(colour = HY2_INK, size = 11),
      plot.margin      = ggplot2::margin(4, 12, 2, 4),
      axis.title       = ggplot2::element_text(colour = HY2_MUTE, size = 11),
      axis.text        = ggplot2::element_text(colour = HY2_MUTE, size = 10.5),
      plot.subtitle    = ggplot2::element_text(colour = HY2_MUTE, size = 10.5)
    )
}

# ---- the three bands, and nothing else -------------------------------------

#' The pathway powers drawn in the simulation plot (W/kg)
#'
#' `aerobic` is the whole muscle flux Z' + Z1'; `lactic` is the whole flow
#' through tube A, wherever in the column it came from; `alactic` is the rate at
#' which P is actually losing stored energy.
#'
#' That last one needs care. `alactic_W` is U' minus the inflows, which is the
#' rate P's content falls -- but ONLY while P has something to give. Once P is
#' empty the level cannot fall further, the ODE clamps dQ_P at zero, and the
#' shortfall is demand that is simply NOT BEING MET. Counting it as alactic
#' credits the phosphagens with energy they did not supply: on a 100 m with
#' P = 150 J/kg and A'max = 25 that inflated the alactic total to 331 J/kg out
#' of a 150 J/kg reservoir. It is returned separately as `unmet`, which is what
#' `deficit_W` has always held.
hyd2_bands <- function(d) {
  want <- pmax(d$alactic_W, 0)
  # What P can actually give over this step: its remaining content divided by the
  # step length. Away from the floor this is enormous and the cap does nothing;
  # at the floor it is zero. Capping by the CONTENT rather than testing a level
  # threshold also gets the one step where P runs out exactly right -- a
  # threshold let that step through whole and overshot C_P by a few tenths.
  dt   <- c(diff(d$time), utils::tail(diff(d$time), 1))
  give <- pmax(d$Q_P, 0) / pmax(dt, 1e-9)
  alac <- pmin(want, give)
  list(aerobic = d$vo2_muscle_W,
       lactic  = pmax(d$Ap, 0),
       alactic = alac,
       unmet   = want - alac)
}

# the three that are pathways; `unmet` is a shortfall, not a supply
HY2_PATHWAYS <- c("aerobic", "lactic", "alactic")

#' Running time integral (J/kg) of a power series
hyd2_trap <- function(x, t) {
  n <- length(t)
  if (n < 2) return(rep(0, n))
  c(0, cumsum(0.5 * (x[-1] + x[-n]) * diff(t)))
}

#' Attach the cumulative energies to a run, at FULL resolution
#'
#' The plots plot a down-sampled copy so playback stays at 10 fps, but an
#' integral taken over 200 points is not the integral over 1600 — and the
#' head-line number must be the area of the band that is drawn. Integrating once,
#' here, and carrying the result through the down-sampling keeps them identical.
hyd2_attach_cum <- function(d) {
  cm <- lapply(hyd2_bands(d), hyd2_trap, t = d$time)
  d$cum2_aerobic <- cm$aerobic
  d$cum2_lactic  <- cm$lactic
  d$cum2_alactic <- cm$alactic
  d$cum2_unmet   <- cm$unmet
  # the demand too: on a square wave a 200-point grid straddles the step edges,
  # so re-integrating the down-sampled copy would not give the same envelope
  d$cum2_demand  <- hyd2_trap(d$U, d$time)
  d
}

#' Cumulative energy supplied by each pathway (J/kg), band by band
hyd2_cum <- function(d) {
  if (!is.null(d$cum2_aerobic))
    return(list(aerobic = d$cum2_aerobic, lactic = d$cum2_lactic,
                alactic = d$cum2_alactic,
                unmet = if (is.null(d$cum2_unmet)) rep(0, nrow(d)) else d$cum2_unmet))
  lapply(hyd2_bands(d), hyd2_trap, t = d$time)
}

#' Total energy from each pathway over the whole run (J/kg), plus the shares
hyd2_energies <- function(d) {
  cm  <- hyd2_cum(d)
  E   <- vapply(cm, function(x) x[length(x)], numeric(1))
  # the shares are of what was SUPPLIED. Unmet demand is reported beside them,
  # never inside them: it is energy nothing provided.
  sup <- E[HY2_PATHWAYS]
  tot <- sum(sup)
  list(aerobic = E[["aerobic"]], lactic = E[["lactic"]], alactic = E[["alactic"]],
       unmet = if ("unmet" %in% names(E)) E[["unmet"]] else 0,
       total = tot,
       share = if (tot > 0) 100 * sup / tot
               else c(aerobic = NA, lactic = NA, alactic = NA))
}


# ---- pathway powers ---------------------------------------------------------

#' Stacked pathway powers, revealed up to `tnow`
#'
#' @param d Simulation output (down-sampled for playback).
#' @param tnow Playback clock (s).
#' @param ymax Fixed y limit, so the axis does not jump between frames.
#' @param reference Optional case-figure partition (mL O2/kg/min, above rest)
#'   drawn over the stack, in the same stacking order, so each dashed line can be
#'   read against the top of the band beneath it. Its topmost line is the
#'   published TOTAL of the three supplies, which equals the demand for a Gastin
#'   case and falls about a per cent short of it for a sprint case -- there the
#'   demand is a measurement and the three curves are a fit to it.
hyd2_power_plot <- function(d, tnow, ymax = NULL, reference = NULL) {
  b <- hyd2_bands(d)
  if (is.null(ymax))
    ymax <- max(c(d$U, b$aerobic + b$lactic + b$alactic + b$unmet), na.rm = TRUE) * 1.06
  keep <- d$time <= tnow

  gline <- NULL
  if (!is.null(reference)) {
    k <- reference$time <= tnow
    cvt <- 20.9 / 60
    gline <- rbind(
      data.frame(time = reference$time[k], y = reference$aerobic[k] * cvt,
                 who = "Aerobic"),
      data.frame(time = reference$time[k],
                 y = (reference$aerobic[k] + reference$glyco[k]) * cvt,
                 who = "Lactic"),
      data.frame(time = reference$time[k],
                 y = (reference$aerobic[k] + reference$glyco[k] +
                        reference$atp_pcr[k]) * cvt,
                 who = "Alactic"))
  }

  band <- function(lo, hi, lab) data.frame(time = d$time[keep], lo = lo[keep],
                                           hi = hi[keep], pathway = lab)
  sup <- b$aerobic + b$lactic + b$alactic
  st <- rbind(
    band(rep(0, nrow(d)),        b$aerobic,            "Aerobic"),
    band(b$aerobic,              b$aerobic + b$lactic, "Lactic"),
    band(b$aerobic + b$lactic,   sup,                  "Alactic"))
  # Drawn only when there IS a shortfall, so an ordinary run looks exactly as
  # before. When P is empty and the inflows cannot cover U' the stack stops
  # short of the demand, and this band is that gap: the effort is not being
  # sustained. It used to be swept into the alactic band.
  has_unmet <- any(b$unmet > 1e-9)
  if (has_unmet) st <- rbind(st, band(sup, sup + b$unmet, "Unmet"))
  st$pathway <- factor(st$pathway, levels = c(names(HY2_PAL), "Unmet"))
  fills  <- c(HY2_PAL, "Unmet" = HY2_UNMET)
  labs   <- c(HY2_LEGEND, "Unmet" = HY2_UNMET_LAB)
  if (!has_unmet) { fills <- fills[names(HY2_PAL)]; labs <- labs[names(HY2_PAL)] }

  ggplot2::ggplot() +
    ggplot2::geom_line(data = d, ggplot2::aes(.data$time, .data$U),
                       colour = "grey84", linewidth = 0.5) +
    {if (sum(keep) >= 2) ggplot2::geom_ribbon(
      data = st, ggplot2::aes(x = .data$time, ymin = .data$lo, ymax = .data$hi,
                              fill = .data$pathway), alpha = 0.85)} +
    {if (sum(keep) >= 2) ggplot2::geom_line(
      data = d[keep, ], ggplot2::aes(.data$time, .data$U),
      colour = HY2_INK, linewidth = 0.6)} +
    {if (!is.null(gline)) ggplot2::geom_line(
      data = gline, ggplot2::aes(.data$time, .data$y, colour = .data$who),
      linetype = "22", linewidth = 0.7, show.legend = FALSE)} +
    ggplot2::geom_vline(xintercept = tnow, colour = "grey55",
                        linetype = "dashed", linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = fills, labels = labs, drop = TRUE) +
    # only declared when a case figure actually puts a colour aesthetic on the
    # plot; otherwise ggplot warns that no level of the manual scale is used
    {if (!is.null(gline)) ggplot2::scale_colour_manual(values = HY2_PAL, guide = "none")} +
    ggplot2::coord_cartesian(xlim = range(d$time), ylim = c(0, ymax), expand = FALSE) +
    ggplot2::labs(x = NULL, y = "pathway power  (W·kg⁻¹)", fill = NULL,
                  # unexplained dashed lines on someone else's plot are noise
                  subtitle = if (!is.null(gline))
                    "dashed — the published partition of this same demand") +
    theme_hy2()
}


# ---- cumulative energy ------------------------------------------------------

#' Time integral of each pathway, one line each
#'
#' The form of di Prampero's Fig. 13.3: energy expenditure against time, one
#' curve per source, not stacked. Read off the end of a curve and you have that
#' pathway's total; read the gap between two curves at any instant and you have
#' how far apart they are AT THAT INSTANT, which a stack cannot show because
#' every layer above the first is drawn on a moving baseline.
hyd2_cum_plot <- function(d, tnow, ymax = NULL) {
  cm  <- hyd2_cum(d)
  dem <- if (!is.null(d$cum2_demand)) d$cum2_demand else hyd2_trap(d$U, d$time)
  if (is.null(ymax)) ymax <- max(c(dem, cm$aerobic, cm$lactic, cm$alactic),
                                 na.rm = TRUE) * 1.06
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1
  keep <- d$time <= tnow

  # unname(): HY2_LEGEND is a NAMED vector, and ggplot reads the names off a
  # named `breaks` and uses THEM as the legend labels -- which silently replaced
  # "aerobic  Z′+Z₁′" with "Aerobic" and left the demand entry blank.
  keys <- unname(c("demand  U′", HY2_LEGEND[c("Aerobic", "Lactic", "Alactic")]))
  cols <- stats::setNames(c(HY2_INK, HY2_PAL[["Aerobic"]], HY2_PAL[["Lactic"]],
                            HY2_PAL[["Alactic"]]), keys)
  ln <- rbind(
    data.frame(time = d$time, y = dem,        who = keys[1]),
    data.frame(time = d$time, y = cm$aerobic, who = keys[2]),
    data.frame(time = d$time, y = cm$lactic,  who = keys[3]),
    data.frame(time = d$time, y = cm$alactic, who = keys[4]))
  # only when there is one: the running total of demand nothing supplied
  if (!is.null(cm$unmet) && any(cm$unmet > 1e-9)) {
    keys <- c(keys, HY2_UNMET_LAB)
    cols <- c(cols, stats::setNames(HY2_UNMET, HY2_UNMET_LAB))
    ln <- rbind(ln, data.frame(time = d$time, y = cm$unmet, who = HY2_UNMET_LAB))
  }
  ln$who <- factor(ln$who, levels = keys)

  ggplot2::ggplot() +
    # the whole run in grey so the axes never move during playback
    ggplot2::geom_line(data = ln, ggplot2::aes(.data$time, .data$y, group = .data$who),
                       colour = "grey88", linewidth = 0.5) +
    {if (sum(keep) >= 2) ggplot2::geom_line(
      data = ln[rep(keep, nlevels(ln$who)), ],
      ggplot2::aes(.data$time, .data$y, colour = .data$who), linewidth = 1)} +
    ggplot2::geom_vline(xintercept = tnow, colour = "grey55",
                        linetype = "dashed", linewidth = 0.35) +
    ggplot2::scale_colour_manual(values = cols, breaks = keys, drop = FALSE) +
    ggplot2::coord_cartesian(xlim = range(d$time), ylim = c(0, ymax), expand = FALSE) +
    ggplot2::labs(x = "time  (s)", y = "energy expenditure  (J·kg⁻¹)", colour = NULL) +
    theme_hy2()
}


# ---- pathway powers, side by side rather than stacked -----------------------

#' Each pathway's power against time, unstacked
#'
#' The same three series as the supply figure, drawn from a common zero. Stacked
#' bands answer "what made up the demand"; these answer "what was each pathway
#' doing", which is the question you ask when you want to see a peak, a decay
#' rate, or a crossover — none of which survive a moving baseline.
hyd2_power_lines_plot <- function(d, tnow = NULL, ymax = NULL) {
  b <- hyd2_bands(d)
  if (is.null(ymax)) ymax <- max(c(d$U, b$aerobic, b$lactic, b$alactic),
                                 na.rm = TRUE) * 1.06
  keys <- unname(c("demand  U′", HY2_LEGEND[c("Aerobic", "Lactic", "Alactic")]))
  ln <- rbind(
    data.frame(time = d$time, y = d$U,       who = keys[1]),
    data.frame(time = d$time, y = b$aerobic, who = keys[2]),
    data.frame(time = d$time, y = b$lactic,  who = keys[3]),
    data.frame(time = d$time, y = b$alactic, who = keys[4]))
  ln$who <- factor(ln$who, levels = keys)
  cols <- stats::setNames(c(HY2_INK, HY2_PAL[["Aerobic"]], HY2_PAL[["Lactic"]],
                            HY2_PAL[["Alactic"]]), keys)

  ggplot2::ggplot(ln, ggplot2::aes(.data$time, .data$y, colour = .data$who)) +
    ggplot2::geom_line(linewidth = 1) +
    {if (!is.null(tnow)) ggplot2::geom_vline(
      xintercept = tnow, colour = "grey55", linetype = "dashed", linewidth = 0.35)} +
    ggplot2::scale_colour_manual(values = cols, breaks = keys) +
    ggplot2::coord_cartesian(xlim = range(d$time), ylim = c(0, ymax), expand = FALSE) +
    ggplot2::labs(x = "time  (s)", y = "power  (W·kg⁻¹)", colour = NULL) +
    theme_hy2()
}
