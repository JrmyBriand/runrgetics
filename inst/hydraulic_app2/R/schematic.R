# =============================================================================
# ggplot rendering of di Prampero's Fig. I-6.1 for the Shiny app.
#
# The geometry is DRIVEN BY THE MODEL PARAMETERS, and — importantly — the drawn
# WIDTH of each tank is its cross-sectional area profile, i.e. the very thing that
# converts stored energy into fluid LEVEL. So the shapes are not decoration: the
# eLa wedge and the OS taper are the reason those two reservoirs contribute the
# way they do.
#
# As in Fig. I-6.1 there are only two reference levels, L0 (rest) and L1. L1 is
# where the Z / Z1 tubes sit AND the rim of Gl, on which the eLa wedge stands.
# Gl and eLa form ONE connected column, so it drains from the top (eLa first) and
# refills from the bottom (Gl first). The A tube sits at the bottom, as drawn in
# the book, and needs no threshold: flow follows the head between the column and P.
#
# Water is drawn by CLIPPING each tank's own polygon at the water surface, so the
# fill follows the tank shape exactly rather than being a rectangle inside a
# shaped outline.
# =============================================================================

PAL <- c(Ox = "#0072B2", P = "#009E73", OS = "#E69F00",
         Gl = "#D55E00", eLa = "#CC79A7", demand = "#111111", ink = "#1b1b1b")


# Clip closed polygon `pts` (data.frame x,y) to the part at or below `ylim`.
hyd_clip_below <- function(pts, ylim) {
  x <- pts$x; y <- pts$y; n <- length(x)
  ox <- numeric(0); oy <- numeric(0)
  for (i in seq_len(n)) {
    j <- if (i == n) 1L else i + 1L
    if (y[i] <= ylim) { ox <- c(ox, x[i]); oy <- c(oy, y[i]) }
    if ((y[i] <= ylim) != (y[j] <= ylim)) {
      t <- (ylim - y[i]) / (y[j] - y[i])
      ox <- c(ox, x[i] + t * (x[j] - x[i])); oy <- c(oy, ylim)
    }
  }
  if (length(ox) < 3) return(NULL)
  data.frame(x = ox, y = oy)
}


#' Geometry of the five tanks for the current level settings
#'
#' @param p Parameter list (uses `ell1`, `ela_taper`, and the OS curve).
#' @return A list of tank polygons plus the key y-coordinates.
hyd_geometry <- function(p) {
  Y0 <- 6; YT <- 88                      # P floor and the L0 (rest) level
  yl <- function(l) Y0 + l * (YT - Y0)   # level (0..1) -> y
  y_L1  <- yl(p$ell1)
  y_top <- YT + 12                       # tanks are drawn open at the top

  # x layout, following the order of Fig. I-6.1
  ox_x <- c(0, 27); zx <- c(27, 35); px <- c(35, 49); z1x <- c(49, 57)
  os_x0 <- 57; os_wmax <- 17
  gl_x  <- c(82, 118); ela_x1 <- 118; ela_wmax <- 15

  # --- OS: vertical LEFT wall, right boundary follows the width profile ------
  # Narrow neck at the top opening into a bulge that tapers to a point at L1 —
  # di Prampero's shape, and the slope of the venous dissociation curve.
  pr <- p$prof_OS
  os_poly <- data.frame(
    x = c(rep(os_x0, length(pr$h)), rev(os_x0 + os_wmax * pr$w_rel)),
    y = c(yl(pr$h), rev(yl(pr$h))))

  # --- eLa: vertical RIGHT wall, left boundary follows the wedge profile -----
  pe <- p$prof_eLa
  ela_poly <- data.frame(
    x = c(rev(ela_x1 - ela_wmax * pe$w_rel), rep(ela_x1, length(pe$h))),
    y = c(rev(yl(pe$h)), yl(pe$h)))

  tanks <- list(
    # Ox has NO left wall in the book: it is effectively infinite.
    Ox  = data.frame(x = c(ox_x[1], ox_x[1], ox_x[2], ox_x[2]),
                     y = c(YT + 1, y_L1, y_L1, YT + 1)),
    # P is drawn from its own width profile: a plain cylinder when p_taper = 1,
    # di Prampero's tank with a wider base when p_taper < 1 (the conic variant).
    P   = local({
      pp <- p$prof_P; xc <- mean(px); hw <- diff(px) / 2
      w  <- hw * pp$w_rel
      # open at the top, so the path runs down one wall, across, and up the other
      data.frame(x = c(rev(xc - w), xc + w), y = c(rev(yl(pp$h)), yl(pp$h)))
    }),
    OS  = os_poly,
    Gl  = data.frame(x = c(gl_x[1], gl_x[1], gl_x[2], gl_x[2]),
                     y = c(y_L1, Y0, Y0, y_L1)),
    eLa = ela_poly
  )
  list(tanks = tanks, Y0 = Y0, YT = YT, y_top = y_top, y_L1 = y_L1, yl = yl,
       ox_x = ox_x, px = px, zx = zx, z1x = z1x,
       os_x0 = os_x0, gl_x = gl_x, ela_x1 = ela_x1,
       gl_x0 = mean(gl_x), gl_hw = diff(gl_x) / 2)
}


#' Draw the hydraulic schematic at one instant
#'
#' @param row One row of [hyd_simulate()] output (the current time point).
#' @param p Parameter list.
#' @param show_labels Draw the tank/tube labels (default TRUE).
#' @return A ggplot object.
hyd_schematic <- function(row, p, show_labels = TRUE) {
  g  <- hyd_geometry(p)
  tk <- g$tanks
  yl <- g$yl

  # ---- water surfaces (per tank) ----
  # Gl and eLa share ONE surface: the column level h_LA. Above the Gl rim the
  # surface is inside the wedge, below it inside the Gl body — so eLa visibly
  # empties before Gl is touched, and refills only after Gl is full.
  surf_of <- list(
    Ox  = g$YT,                              # ~infinite reservoir, held at L0
    P   = g$yl(row$lvl_P),
    OS  = g$yl(row$h_OS),                    # the ODC level the tank was drawn from
    Gl  = g$yl(min(row$h_LA, p$ell1)),       # column level, clipped to the Gl body
    eLa = g$yl(max(row$h_LA, p$ell1))        # ... and to the wedge above it
  )
  water <- list()
  for (nm in names(tk)) {
    w <- hyd_clip_below(tk[[nm]], surf_of[[nm]])
    if (!is.null(w)) { w$tank <- nm; water[[nm]] <- w }
  }
  water_df <- if (length(water)) do.call(rbind, water) else NULL

  outline_df <- do.call(rbind, lapply(names(tk), function(n) {
    d <- tk[[n]]; d$tank <- n; d
  }))

  # ---- tubes ----
  seg <- function(x1, y1, x2, y2) data.frame(x = x1, y = y1, xend = x2, yend = y2)
  # The A tube runs along the BOTTOM, under OS, exactly as di Prampero draws it.
  # It needs no threshold: flow follows the head between the column and P.
  y_A  <- g$Y0 + 2.5
  glL  <- g$gl_x[1]
  tubes <- rbind(
    seg(g$zx[1],  g$y_L1, g$zx[2],  g$y_L1),   # Z  : Ox -> P (the L1 floor line)
    seg(g$z1x[1], g$y_L1, g$z1x[2], g$y_L1),   # Z1 : OS <-> P
    seg(g$px[2],  y_A,    glL,      y_A)       # A  : column -> P, along the bottom
  )

  # ---- flow arrows: width/alpha ~ |flow| ----
  amp <- function(v, s) min(abs(v) / max(s, 1e-9), 1)
  Ap_scale <- max(p$Amax * 0.6, 1e-9)
  arr <- rbind(
    data.frame(x = g$zx[1] + 0.5, y = g$y_L1, xend = g$zx[2] - 0.5, yend = g$y_L1,
               m = amp(row$Zp, p$Zmax), col = PAL[["Ox"]]),
    if (row$Z1p >= 0)
      data.frame(x = g$z1x[2] - 0.5, y = g$y_L1, xend = g$z1x[1] + 0.5, yend = g$y_L1,
                 m = amp(row$Z1p, p$Zmax * 0.5), col = PAL[["OS"]])
    else
      data.frame(x = g$z1x[1] + 0.5, y = g$y_L1, xend = g$z1x[2] - 0.5, yend = g$y_L1,
                 m = amp(row$Z1p, p$Zmax * 0.5), col = PAL[["OS"]]),
    # A: colour shows WHICH part of the column is supplying the flow — the eLa
    # wedge (early lactate) while the level is above the Gl rim, the Gl body below.
    if (row$Ap >= 0)
      data.frame(x = glL - 1, y = y_A, xend = g$px[2] + 1, yend = y_A,
                 m = amp(row$Ap, Ap_scale),
                 col = if (row$eLap > 0) PAL[["eLa"]] else PAL[["Gl"]])
    else
      data.frame(x = g$px[2] + 1, y = y_A, xend = glL - 1, yend = y_A,
                 m = amp(row$Ap, Ap_scale), col = PAL[["Gl"]]),
    # outflow U' below the tap
    data.frame(x = mean(g$px), y = g$Y0 - 5, xend = mean(g$px), yend = g$Y0 - 14,
               m = amp(row$U, p$Zmax * 1.5), col = PAL[["demand"]])
  )
  arr$alpha <- pmax(arr$m, 0.06)
  arr$lw    <- 0.5 + 3.2 * arr$m

  # Only L0 and L1 exist in Fig. I-6.1 — and only these two are needed by the
  # equations. Sorted ascending: ggplot orders axis breaks, labels must follow.
  lvl_lines <- data.frame(y = c(g$y_L1, g$YT), lab = c("L₁", "L₀"))
  lvl_lines <- lvl_lines[order(lvl_lines$y), ]

  pl <- ggplot2::ggplot() +
    # level reference lines
    ggplot2::geom_hline(data = lvl_lines, ggplot2::aes(yintercept = .data$y),
                        linetype = "dashed", colour = "grey62", linewidth = 0.3) +
    # water (clipped to each tank's shape)
    {if (!is.null(water_df))
      ggplot2::geom_polygon(data = water_df,
                            ggplot2::aes(.data$x, .data$y, group = .data$tank,
                                         fill = .data$tank), alpha = 0.62)} +
    # tank outlines (open tops)
    ggplot2::geom_path(data = outline_df,
                       ggplot2::aes(.data$x, .data$y, group = .data$tank),
                       colour = PAL[["ink"]], linewidth = 0.6) +
    # tubes
    ggplot2::geom_segment(data = tubes,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       xend = .data$xend, yend = .data$yend),
                          colour = PAL[["ink"]], linewidth = 0.45) +
    # tap S below P
    ggplot2::geom_segment(ggplot2::aes(x = mean(g$px), y = g$Y0,
                                       xend = mean(g$px), yend = g$Y0 - 5),
                          colour = PAL[["ink"]], linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = mean(g$px), y = g$Y0 - 5), shape = 23,
                        size = 2.6, fill = "white", colour = PAL[["ink"]], stroke = 0.5) +
    # flow arrows
    ggplot2::geom_segment(
      data = arr,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      colour = arr$col, alpha = arr$alpha, linewidth = arr$lw,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.16, "cm"), type = "closed"),
      lineend = "round") +
    ggplot2::scale_fill_manual(values = PAL, guide = "none") +
    ggplot2::scale_y_continuous(
      breaks = lvl_lines$y, labels = lvl_lines$lab,
      expand = ggplot2::expansion(mult = c(0.06, 0.04))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::coord_cartesian(ylim = c(g$Y0 - 16, g$y_top + 8), clip = "off") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = "grey45", size = 8, hjust = 1),
      plot.margin = ggplot2::margin(6, 10, 4, 4))

  if (show_labels) {
    labs <- data.frame(
      x = c(mean(g$ox_x) - 2, mean(g$px), g$os_x0 + 9, mean(g$gl_x), g$ela_x1 - 6,
            mean(g$zx), mean(g$z1x), mean(c(g$px[2], g$gl_x[1])), mean(g$px) + 4),
      y = c(g$y_L1 + 14, g$y_top + 4, g$y_top + 4, (g$Y0 + g$y_L1) / 2, g$y_top + 4,
            g$y_L1 - 3.6, g$y_L1 - 3.6, y_A + 3.2, g$Y0 - 11),
      lab = c("Ox", "P", "OS", "Gl", "eLa", "Z", "Z\u2081", "A", "U\u02b9"),
      col = c(PAL[["Ox"]], PAL[["P"]], PAL[["OS"]], PAL[["Gl"]], PAL[["eLa"]],
              PAL[["Ox"]], PAL[["OS"]], PAL[["Gl"]], PAL[["demand"]]),
      sz  = c(4.1, 4.1, 4.1, 4.1, 4.1, 3.1, 3.1, 3.1, 3.4),
      fc  = c("bold", "bold", "bold", "bold", "bold",
              "plain", "plain", "plain", "bold")
    )
    pl <- pl + ggplot2::geom_text(
      data = labs,
      ggplot2::aes(.data$x, .data$y, label = .data$lab),
      colour = labs$col, size = labs$sz, fontface = labs$fc)
  }
  pl
}


# =============================================================================
# Combined live frame: the hydraulic schematic above, the pathway powers below,
# both drawn at the SAME instant. Rendering them as one plot guarantees they stay
# in step during playback and costs one render per frame instead of two.
# =============================================================================

#' Pathway-power panel, revealed up to `tnow`
#'
#' @param d Simulation output (already down-sampled for plotting).
#' @param tnow Current time (s).
#' @param p Parameter list.
#' @param ymax Fixed y limit so the axis does not jump between frames.
hyd_power_panel <- function(d, tnow, p, ymax = NULL, gastin = NULL) {
  aer <- d$vo2_muscle_W
  lac <- pmax(d$Ap, 0)                      # A' is the WHOLE lactic flow
  ala <- pmax(d$alactic_W, 0)
  if (is.null(ymax)) ymax <- max(c(d$U, aer + lac + ala), na.rm = TRUE) * 1.06
  keep <- d$time <= tnow

  # Gastin's own partition of the same demand, for direct comparison. His curves
  # are in mL O2/kg/min; the panel is in W/kg, so convert with EQ_O2.
  gline <- NULL
  if (!is.null(gastin)) {
    k <- gastin$time <= tnow
    gline <- rbind(
      data.frame(time = gastin$time[k], y = gastin$aerobic[k] * 20.9 / 60,
                 who = "Aerobic  Z\u2032+Z\u2081\u2032"),
      data.frame(time = gastin$time[k],
                 y = (gastin$aerobic[k] + gastin$glyco[k]) * 20.9 / 60,
                 who = "Lactic  A\u2032"),
      data.frame(time = gastin$time[k],
                 y = (gastin$aerobic[k] + gastin$glyco[k] + gastin$atp_pcr[k]) * 20.9 / 60,
                 who = "Alactic  (PCr\u2193)"))
  }

  band <- function(lo, hi, lab) data.frame(time = d$time[keep], lo = lo[keep],
                                           hi = hi[keep], pathway = lab)
  st <- rbind(
    band(rep(0, nrow(d)), aer,             "Aerobic  Z′+Z₁′"),
    band(aer,             aer + lac,       "Lactic  A′"),
    band(aer + lac,       aer + lac + ala, "Alactic  (PCr↓)"))
  st$pathway <- factor(st$pathway, levels = c("Aerobic  Z′+Z₁′",
                                              "Lactic  A′",
                                              "Alactic  (PCr↓)"))
  ggplot2::ggplot() +
    # full demand trace in grey so the axes never move, then the filled areas
    ggplot2::geom_line(data = d, ggplot2::aes(.data$time, .data$U),
                       colour = "grey80", linewidth = 0.5) +
    {if (sum(keep) >= 2) ggplot2::geom_ribbon(
      data = st, ggplot2::aes(x = .data$time, ymin = .data$lo, ymax = .data$hi,
                              fill = .data$pathway), alpha = 0.8)} +
    {if (sum(keep) >= 2) ggplot2::geom_line(
      data = d[keep, ], ggplot2::aes(.data$time, .data$U),
      colour = "#111111", linewidth = 0.6)} +
    # Gastin's cumulative boundaries, drawn as dashed lines on the same stack:
    # where his line sits above the band edge, the model gave that pathway less.
    {if (!is.null(gline)) ggplot2::geom_line(
      data = gline, ggplot2::aes(.data$time, .data$y, colour = .data$who),
      linetype = "22", linewidth = 0.7, show.legend = FALSE)} +
    ggplot2::geom_vline(xintercept = tnow, colour = "grey45",
                        linetype = "dashed", linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
    ggplot2::scale_colour_manual(values = c("Aerobic  Z\u2032+Z\u2081\u2032" = "#0072B2",
                                            "Lactic  A\u2032" = "#D55E00",
                                            "Alactic  (PCr\u2193)" = "#009E73"),
                                 guide = "none") +
    ggplot2::coord_cartesian(xlim = range(d$time), ylim = c(0, ymax), expand = FALSE) +
    ggplot2::labs(x = "time (s)", y = "pathway power  (W·kg⁻¹)", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top", legend.margin = ggplot2::margin(0, 0, 0, 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey91", linewidth = 0.3),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      axis.title = ggplot2::element_text(colour = "grey30", size = 10),
      axis.text  = ggplot2::element_text(colour = "grey40", size = 9))
}

#' One synchronised frame: schematic + pathway powers
#' @export
hyd_live_plot <- function(d, tnow, p, ymax = NULL) {
  row <- d[which.min(abs(d$time - tnow)), , drop = FALSE]
  patchwork::wrap_plots(
    hyd_schematic(row, p),
    hyd_power_panel(d, tnow, p, ymax = ymax),
    ncol = 1, heights = c(1.1, 1))
}
