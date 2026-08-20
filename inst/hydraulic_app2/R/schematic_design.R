# =============================================================================
# The schematic AS A CONTROL SURFACE.
#
# Draws di Prampero's Fig. I-6.1 from a design (see designer.R) rather than from
# model parameters: each reservoir appears at the height and width you gave it,
# and its area IS its energy.
#
# hyd_design_layout() is the single source of truth for the geometry. It returns
# both the drawing coordinates and an ANCHOR for every adjustable dimension, in
# data units, next to the part of the figure it controls. app.R turns those
# anchors into real HTML +/- buttons laid over the plot -- real buttons, because
# ggplot's click coordmap is unusable under theme_void (it reports normalised,
# not data, coordinates).
# =============================================================================

#' Geometry of the configurable schematic
#'
#' @param d Design list from [design_defaults()].
#' @return A list of drawing coordinates, the exact panel limits (`xlim`,
#'   `ylim`) and an `anchors` data frame (`id`, `x`, `y`, `lab`, `col`).
hyd_design_layout <- function(d) {
  Y0 <- 6; YT <- 88
  yl <- function(l) Y0 + l * (YT - Y0)
  y_top <- YT + 10
  y_L1 <- yl(d$ell1); y_L2 <- yl(d$ell2)

  SCX  <- 1
  ox_w <- 26
  gap  <- 16          # room for the energy scale up P's left wall
  ox_x <- c(0, ox_w)
  # P widens/narrows SYMMETRICALLY about its centre line, so changing either
  # width moves both walls and the tank stays vertical. `px` is its bounding box.
  w_P_max <- SCX * max(d$w_P, d$w_P_top)
  p_cx  <- ox_x[2] + gap + w_P_max / 2
  px    <- c(p_cx - w_P_max / 2, p_cx + w_P_max / 2)
  # half-width of P at a given y, and the wall positions the tubes must meet
  p_hw    <- function(y) SCX * (d$w_P + (d$w_P_top - d$w_P) *
                                 (y - Y0) / (y_top - Y0)) / 2
  p_wallL <- function(y) p_cx - p_hw(y)
  p_wallR <- function(y) p_cx + p_hw(y)
  os_x0 <- px[2] + gap
  os_w  <- SCX * max(d$w_OS_bot, d$w_OS_neck, d$w_OS_body)
  col_x0 <- os_x0 + os_w + gap                       # lactic column, left edge
  # Gl is a trapezoid: w_Gl at the floor, w_Gl_top at the RIM.
  w_Gl_top <- if (is.null(d$w_Gl_top)) d$w_Gl else d$w_Gl_top
  # The column is RIGHT-ALIGNED: Gl and eLa share one vertical wall on the RIGHT
  # and every width is taken off to the LEFT of it. That keeps the outline the
  # energy scales sit against straight, and puts all the shaping on one side.
  col_xR   <- col_x0 + SCX * max(d$w_Gl, w_Gl_top, d$w_eLa_bot, d$w_eLa_top)
  gl_rim_x <- col_xR - SCX * w_Gl_top                # Gl's LEFT wall at the rim
  z_mid  <- d$ell2 + d$gl_h
  z_top  <- min(z_mid + d$ela_h, 1)
  # where the column's sloping left wall stands at a given level: the A tube has
  # to meet it, and it moves when the tube floats
  col_wallL <- function(lev) {
    w <- if (lev >= z_mid) {
      u <- (min(lev, z_top) - z_mid) / max(z_top - z_mid, 1e-9)
      d$w_eLa_bot + (d$w_eLa_top - d$w_eLa_bot) * min(max(u, 0), 1)
    } else {
      v <- (max(lev, d$ell2) - d$ell2) / max(d$gl_h, 1e-9)
      d$w_Gl + (w_Gl_top - d$w_Gl) * min(max(v, 0), 1)
    }
    col_xR - SCX * w
  }
  span   <- 1 - d$ell1
  z_neck <- 1 - d$os_neck_h * span                   # where the OS neck begins

  xlim <- c(-6, col_xR + 42)
  ylim <- c(Y0 - 18, y_top + 8)

  # ---- one anchor per adjustable dimension, beside what it controls ---------
  a <- function(id, x, y, lab, col) data.frame(id = id, x = x, y = y,
                                               lab = lab, col = col,
                                               stringsAsFactors = FALSE)
  x_os  <- os_x0 + os_w + 15                         # empty band right of OS
  x_ela <- col_xR + 26                               # clear of the column's scales
  x_col <- col_xR - 0.70 * SCX * d$w_Gl              # over the Gl block
  anchors <- rbind(
    a("vo2m",      mean(ox_x),  y_L1 - 12,                 "VO\u2082max",            PAL[["Ox"]]),
    a("ell1",      mean(ox_x),  y_L1 + 12,                 "L\u2081 (Z\u2032 saturates)", PAL[["Ox"]]),
    a("wP",        mean(px),    Y0 - 9,                    "P width at base",     PAL[["P"]]),
    a("wPt",       mean(px),    y_top + 4,                 "P width at top",      PAL[["P"]]),
    a("wOSn",      x_os,        yl(0.94),                  "OS efflux width",     PAL[["OS"]]),
    a("osNh",      x_os,        yl(z_neck) - 2,            "OS efflux height",    PAL[["OS"]]),
    a("wOSy",      x_os,        yl((d$ell1 + z_neck) / 2), "OS body width",       PAL[["OS"]]),
    a("wOSb",      x_os,        y_L1 + 5,                  "OS width at L\u2081",  PAL[["OS"]]),
    a("wElat",     x_ela,       yl(z_top) - 4,             "eLa upper width",     PAL[["eLa"]]),
    a("elaH",      x_ela,       yl((z_mid + z_top) / 2),   "eLa height",          PAL[["eLa"]]),
    a("wElab",     x_ela,       yl(z_mid) + 9,             "eLa lower width",     PAL[["eLa"]]),
    a("wGlt",      x_col,       yl(z_mid) - 6,             "Gl width at rim",     PAL[["Gl"]]),
    a("glH",       x_col,       yl((d$ell2 + z_mid) / 2),  "Gl height",           PAL[["Gl"]]),
    a("wGl",       x_col,       yl(d$ell2) + 8,            "Gl width at base",    PAL[["Gl"]]),
    a("ell2",      x_col,       yl(d$ell2) - 9,            "L\u2082 (Gl up/down)", PAL[["Gl"]])
  )

  # Anchors follow the geometry, so a design that narrows a reservoir can slide
  # two clusters into each other. Resolve in panel FRACTIONS (a widget is about
  # 92 x 34 px of a 1150 x 400 px panel): group anchors that share a horizontal
  # band, then enforce the vertical separation a widget needs within each band.
  FW <- 0.085; FH <- 0.090
  nx <- (anchors$x - xlim[1]) / diff(xlim)
  ny <- (anchors$y - ylim[1]) / diff(ylim)
  ord <- order(nx)
  grp <- integer(nrow(anchors)); g <- 1L; grp[ord[1]] <- 1L
  for (i in seq_along(ord)[-1]) {
    if (nx[ord[i]] - nx[ord[i - 1]] > FW) g <- g + 1L
    grp[ord[i]] <- g
  }
  for (gi in unique(grp)) {
    k <- which(grp == gi)
    if (length(k) < 2) next
    k <- k[order(-ny[k])]                       # highest on the figure first
    for (i in seq_along(k)[-1]) {
      if (ny[k[i - 1]] - ny[k[i]] < FH) ny[k[i]] <- ny[k[i - 1]] - FH
    }
    lo <- min(ny[k]); if (lo < FH / 2) ny[k] <- ny[k] + (FH / 2 - lo)
    hi <- max(ny[k]); if (hi > 1 - FH / 2) ny[k] <- ny[k] - (hi - (1 - FH / 2))
  }
  anchors$y <- ylim[1] + ny * diff(ylim)

  list(Y0 = Y0, YT = YT, yl = yl, y_top = y_top, y_L1 = y_L1, y_L2 = y_L2,
       SCX = SCX, ox_x = ox_x, gap = gap, px = px, p_cx = p_cx,
       p_wallL = p_wallL, p_wallR = p_wallR, os_x0 = os_x0, os_w = os_w,
       col_x0 = col_x0, col_xR = col_xR, gl_rim_x = gl_rim_x,
       col_wallL = col_wallL, w_Gl_top = w_Gl_top, z_mid = z_mid, z_top = z_top,
       z_neck = z_neck, xlim = xlim, ylim = ylim, anchors = anchors)
}

#' Energy scale up the side of P
#'
#' P's fluid level is what drives Z′ and A′, but the figure only ever showed the
#' level -- how much ENERGY a given drop corresponds to was left to be guessed,
#' and with a conic P the two are not proportional. This returns tick positions
#' for a ruler reading ENERGY SPENT FROM FULL: 0 at L₀, C_P at the floor.
#'
#' The ticks are placed by integrating P's own width profile, so a cylinder
#' gives evenly spaced ticks (half the height IS half the energy) while a cone
#' bunches them toward the narrow end -- which is the point of drawing a cone.
#'
#' @param d Design list.
#' @param k_area Energy per unit drawn area (`K_AREA`).
#' @return `data.frame(used, h)` -- energy spent (J/kg) and the level it is at.
hyd_p_ruler <- function(d, k_area = K_AREA, n_ticks = 4) {
  n   <- 401
  h   <- seq(0, 1, length.out = n)
  wtop <- if (is.null(d$w_P_top)) d$w_P else d$w_P_top
  w   <- d$w_P + (wtop - d$w_P) * h
  V   <- cumsum(c(0, 0.5 * (w[-1] + w[-n]) * diff(h))) * k_area  # energy below h
  C_P <- V[n]
  if (!is.finite(C_P) || C_P <= 0) return(NULL)
  # a pretty tick within a whisker of the capacity collides with the end label
  ticks <- pretty(c(0, C_P), n = n_ticks)
  ticks <- c(0, ticks[ticks > 0 & ticks < 0.88 * C_P], C_P)
  data.frame(used = ticks,
             h = stats::approx(V, h, xout = pmin(pmax(C_P - ticks, 0), C_P),
                               rule = 2)$y)
}

#' Energy scale for one reservoir of the lactic column
#'
#' Same reading as [hyd_p_ruler()] -- energy SPENT FROM FULL, 0 at that
#' reservoir's own top -- but built for the two parts of the lactic column. The
#' column drains from the top down, so eLa's scale is exhausted before Gl's
#' begins, and the two read consecutively.
#'
#' @param w_fun Width as a function of normalised height (0 at the floor).
#' @param h_lo,h_hi Level span of the reservoir.
#' @param capacity Energy it holds when full (J/kg).
#' @return `data.frame(used, h)` in the same form as [hyd_p_ruler()].
hyd_span_ruler <- function(w_fun, h_lo, h_hi, capacity, n_ticks = 3) {
  if (!is.finite(capacity) || capacity <= 0 || h_hi <= h_lo) return(NULL)
  n <- 401
  u <- seq(0, 1, length.out = n)
  w <- pmax(w_fun(u), 1e-9)
  V <- cumsum(c(0, 0.5 * (w[-1] + w[-n]) * diff(u)))
  V <- V / V[n] * capacity                       # energy below each level
  ticks <- pretty(c(0, capacity), n = n_ticks)
  ticks <- c(0, ticks[ticks > 0 & ticks < 0.88 * capacity], capacity)
  data.frame(used = ticks,
             h = h_lo + (h_hi - h_lo) *
               stats::approx(V, u, xout = pmin(pmax(capacity - ticks, 0), capacity),
                             rule = 2)$y)
}

#' The two energy scales of the lactic column (eLa above, Gl below)
hyd_col_rulers <- function(d, ela_pow = NULL) {
  if (is.null(ela_pow)) ela_pow <- if (is.null(d$ela_pow)) 1 else d$ela_pow
  cap    <- design_capacities(d)
  z_mid  <- d$ell2 + d$gl_h
  z_top  <- min(z_mid + d$ela_h, 1)
  w_top  <- if (is.null(d$w_Gl_top)) d$w_Gl else d$w_Gl_top
  list(
    eLa = hyd_span_ruler(
      function(u) d$w_eLa_bot + (d$w_eLa_top - d$w_eLa_bot) *
        u^(1 / max(ela_pow, 1e-6)),
      z_mid, z_top, cap$eLa),
    Gl = hyd_span_ruler(function(u) d$w_Gl + (w_top - d$w_Gl) * u,
                        d$ell2, z_mid, cap$Gl))
}

#' Draw the configurable schematic
#'
#' @param d Design list from [design_defaults()].
#' @param row Optional one-row simulation output; when supplied the reservoirs
#'   are filled to their current levels, otherwise they are shown full.
#' @param p Optional parameter list matching `d` (for the fluid levels).
#' @param handles Draw the +/- handles (default TRUE).
#' @param ruler Draw the energy scale up P's left wall (see [hyd_p_ruler()]).
#' @return A ggplot, with the handle positions attached as `attr(., "handles")`.
hyd_schematic_design <- function(d, row = NULL, p = NULL, handles = FALSE,
                                 float = FALSE, ela_pow = NULL, os_pow = NULL,
                                 ruler = FALSE) {
  # curvature lives on the design; the arguments are an override for tests
  if (is.null(ela_pow)) ela_pow <- if (is.null(d$ela_pow)) 1 else d$ela_pow
  if (is.null(os_pow))  os_pow  <- if (is.null(d$os_pow))  1 else d$os_pow
  L <- hyd_design_layout(d)
  Y0 <- L$Y0; YT <- L$YT; yl <- L$yl; y_top <- L$y_top
  y_L1 <- L$y_L1; y_L2 <- L$y_L2
  SCX <- L$SCX; ox_x <- L$ox_x; gap <- L$gap; px <- L$px
  p_cx <- L$p_cx; p_wallL <- L$p_wallL; p_wallR <- L$p_wallR
  os_x0 <- L$os_x0; os_w <- L$os_w
  col_x0 <- L$col_x0; col_xR <- L$col_xR
  gl_rim_x <- L$gl_rim_x; w_Gl_top <- L$w_Gl_top; col_wallL <- L$col_wallL
  z_mid <- L$z_mid; z_top <- L$z_top; z_neck <- L$z_neck
  xlim <- L$xlim; ylim <- L$ylim

  seg <- function(x1,y1,x2,y2) data.frame(x=x1,y=y1,xend=x2,yend=y2)
  poly <- function(x,y,tank) data.frame(x=x,y=y,tank=tank)

  # ---- tank outlines --------------------------------------------------------
  # OS: a narrow NECK at the top (the small early efflux, set by the flat upper
  # part of the O2 dissociation curve) over a body that widens then tapers to L1.
  os_h   <- sort(unique(c(seq(d$ell1, z_neck, length.out = 60), z_neck,
                          z_neck + 1e-6, seq(z_neck, 1, length.out = 12))))
  # Three pieces, the same law the model integrates: a narrow TUBE over the top
  # os_neck_h of the span, a FLAT SECTION where it steps out to the body width,
  # and the MAIN RESERVOIR below, whose wall runs y = x^os_pow with the origin at
  # the outer corner of the shelf, x inward and y downward.
  v_os   <- pmin(pmax((z_neck - os_h) / max(z_neck - d$ell1, 1e-9), 0), 1)
  ub_os  <- 1 - v_os                                   # height above L1
  wall   <- if (isTRUE(d$os_from_base))
              d$w_OS_bot + (d$w_OS_body - d$w_OS_bot) * ub_os^(1 / max(os_pow, 1e-6))
            else
              d$w_OS_body - (d$w_OS_body - d$w_OS_bot) * v_os^(1 / max(os_pow, 1e-6))
  os_w_h <- SCX * ifelse(os_h >= z_neck, d$w_OS_neck, wall)
  # eLa's wall uses the SAME law as the model's profile, so the drawing and the
  # simulation cannot disagree. Origin at the base of the wedge, x horizontal,
  # y vertical: the wall is y = x^pow, i.e. the width goes as u^(1/pow).
  ela_h <- seq(z_mid, z_top, length.out = 60)
  u_ela <- (ela_h - z_mid) / max(z_top - z_mid, 1e-9)
  f_ela <- u_ela^(1 / max(ela_pow, 1e-6))
  ela_w <- SCX * pmax(d$w_eLa_bot + (d$w_eLa_top - d$w_eLa_bot) * f_ela, 1e-3)

  tanks <- list(
    Ox  = poly(c(ox_x[1], ox_x[1], ox_x[2], ox_x[2]),
               c(YT + 1, y_L1, y_L1, YT + 1), "Ox"),
    # P: w_P at the floor, w_P_top at the rim, centred on p_cx. Equal widths
    # give di Prampero's cylinder; a narrower top is the conic P.
    P   = poly(c(p_wallL(y_top), p_wallL(Y0), p_wallR(Y0), p_wallR(y_top)),
               c(y_top, Y0, Y0, y_top), "P"),
    OS  = poly(c(rep(os_x0, length(os_h)), rev(os_x0 + os_w_h)),
               c(yl(os_h), rev(yl(os_h))), "OS"),
    Gl  = poly(c(gl_rim_x, col_xR - SCX * d$w_Gl, col_xR, col_xR),
               c(yl(z_mid), yl(d$ell2), yl(d$ell2), yl(z_mid)), "Gl"),
    eLa = poly(c(rev(col_xR - ela_w), rep(col_xR, length(ela_h))),
               c(rev(yl(ela_h)), yl(ela_h)), "eLa")
  )

  # ---- fluid levels ---------------------------------------------------------
  surf <- list(Ox = YT, P = yl(1), OS = yl(1), Gl = yl(z_mid), eLa = yl(z_top))
  if (!is.null(row)) {
    h_LA <- if (!is.null(row$h_LA)) row$h_LA else z_top
    surf$P   <- yl(row$h_P)
    surf$OS  <- yl(if (!is.null(row$h_OS)) row$h_OS else 1)
    surf$Gl  <- yl(min(h_LA, z_mid))
    surf$eLa <- yl(max(h_LA, z_mid))
  }
  water <- list()
  for (nm in names(tanks)) {
    w <- hyd_clip_below(tanks[[nm]][, c("x","y")], surf[[nm]])
    if (!is.null(w)) { w$tank <- nm; water[[nm]] <- w }
  }
  water_df <- if (length(water)) do.call(rbind, water) else NULL
  outline  <- do.call(rbind, tanks)

  # ---- tubes ----------------------------------------------------------------
  y_A <- yl(d$ell2) + 2
  # Z1 FLOATS, and its law always has. It reads
  #     Z1 = Z'max * (h_OS - max(h_P, L1)) / (1 - L1)
  # which is the same form as the floating A: the intake rides on P's surface
  # while P is above L1, and bottoms out at L1, the store's own floor, exactly
  # as A bottoms out at L2, the column's floor. Drawing it pinned at L1 showed
  # only that limiting case and hid the head the model was using. It floats in
  # both tube configurations because the law does not consult `float`; only A's
  # conductance does.
  y_Z1 <- max(surf$P, y_L1)
  # a floating A cannot sink below the column's own floor: there is nothing
  # to draw from under L2, and the flow law clamps the head there too
  y_A_draw <- if (isTRUE(float)) max(surf$P, yl(d$ell2)) else y_A
  # Z1 and A both ride on P, so above L1 they land on the same line, and with OS
  # sitting between them the pair reads as one pipe running P -> OS -> column.
  # Separate them by a hair. The cost is under 1% of the level range, drawn on
  # the tube whose exact height is the less informative of the two.
  if (abs(y_A_draw - y_Z1) < 5) y_A_draw <- max(yl(d$ell2), y_Z1 - 5)
  # A runs from the lactic column to P and passes the OS tank on the way. When
  # it floats it can sit at any height, including across OS, and an unbroken
  # line there reads as though A were plumbed into the O2 store, or as though
  # the L1 tubes had moved. Break it where it crosses, the usual convention for
  # a pipe passing behind something.
  a_x0 <- col_wallL((y_A_draw - Y0) / (YT - Y0))
  a_x1 <- p_wallR(y_A_draw)
  a_lo <- min(a_x0, a_x1); a_hi <- max(a_x0, a_x1)
  os_lo <- os_x0 - 1.5; os_hi <- os_x0 + os_w + 1.5
  # ...but only where the tank actually IS. Its floor is L1 and its top is the
  # vessel's, so a tube running below L1 passes under it and needs no break.
  # Testing the x overlap alone cut the fixed tube A, which sits down at L2, in
  # half for no reason.
  crosses_os <- y_A_draw >= y_L1 - 1e-9 && y_A_draw <= y_top + 1e-9
  a_seg <- if (crosses_os && a_lo < os_hi && a_hi > os_lo)
    rbind(seg(a_lo, y_A_draw, max(a_lo, os_lo), y_A_draw),
          seg(min(a_hi, os_hi), y_A_draw, a_hi, y_A_draw))
  else seg(a_lo, y_A_draw, a_hi, y_A_draw)
  a_seg <- a_seg[a_seg$xend - a_seg$x > 0.5, , drop = FALSE]

  tubes <- rbind(
    seg(ox_x[2], y_L1, p_wallL(y_L1), y_L1),                 # Z, welded at L1
    seg(p_wallR(y_Z1), y_Z1, os_x0, y_Z1),                   # Z1, floats to L1
    a_seg                                                     # A
  )
  # a marker so the moving tubes read as tubes rather than stray rules. Z1 earns
  # one whenever it is actually riding on P, ie above its floor at L1.
  float_marks <- rbind(
    if (y_Z1 > y_L1 + 1e-9) data.frame(x = os_x0 - 3, y = y_Z1) else NULL,
    if (isTRUE(float))
      data.frame(x = col_wallL((y_A_draw - Y0) / (YT - Y0)) - 3,
                 y = y_A_draw) else NULL)
  if (is.data.frame(float_marks) && !nrow(float_marks)) float_marks <- NULL

  # ---- +/- handles ----------------------------------------------------------
  H <- data.frame()
  if (handles) {
    add <- function(id, x, y, lab) data.frame(id = id, x = x, y = y, lab = lab)
    H <- rbind(
      add("ell1_up",   ox_x[2] + gap/2, y_L1 + 7, "+"),
      add("ell1_dn",   ox_x[2] + gap/2, y_L1 - 7, "−"),
      add("wP_up",     mean(px),  Y0 - 8,  "+"),
      add("wP_dn",     mean(px) + 7, Y0 - 8, "−"),
      add("wOSb_up",   os_x0 + os_w + 5, y_L1 + 5,  "+"),
      add("wOSb_dn",   os_x0 + os_w + 5, y_L1 - 2,  "−"),
      add("wOSt_up",   os_x0 + os_w + 5, y_top - 12, "+"),
      add("wOSt_dn",   os_x0 + os_w + 5, y_top - 20, "−"),
      add("elaH_up",   col_xR + 18, yl(z_top) - 4, "+"),
      add("elaH_dn",   col_xR + 18, yl(z_top) - 12, "−"),
      add("wElat_up",  col_xR + 6, yl(z_top) - 4,  "+"),
      add("wElat_dn",  col_xR + 6, yl(z_top) - 12, "−"),
      add("wElab_up",  col_xR + 6, yl(z_mid) + 8,  "+"),
      add("wElab_dn",  col_xR + 6, yl(z_mid) + 1,  "−"),
      add("glH_up",    col_xR + 18, yl(z_mid) - 6, "+"),
      add("glH_dn",    col_xR + 18, yl(z_mid) - 14, "−"),
      add("wGl_up",    col_x0 + SCX*d$w_Gl/2 - 6, yl(d$ell2) - 8, "+"),
      add("wGl_dn",    col_x0 + SCX*d$w_Gl/2 + 6, yl(d$ell2) - 8, "−"),
      add("ell2_up",   col_x0 - 6, yl(d$ell2) + 8, "+"),
      add("ell2_dn",   col_x0 - 6, yl(d$ell2) + 1, "−")
    )
  }

  lvl <- data.frame(y = c(y_L1, YT, y_L2), lab = c("L₁", "L₀", "L₂"))
  lvl <- lvl[order(lvl$y), ]

  pl <- ggplot2::ggplot() +
    ggplot2::geom_hline(data = lvl, ggplot2::aes(yintercept = .data$y),
                        linetype = "dashed", colour = "grey62", linewidth = 0.3) +
    {if (!is.null(water_df)) ggplot2::geom_polygon(
      data = water_df, ggplot2::aes(.data$x, .data$y, group = .data$tank,
                                    fill = .data$tank), alpha = 0.6)} +
    ggplot2::geom_path(data = outline,
                       ggplot2::aes(.data$x, .data$y, group = .data$tank),
                       colour = PAL[["ink"]], linewidth = 0.6) +
    {if (!is.null(float_marks)) ggplot2::geom_point(
      data = float_marks, ggplot2::aes(.data$x, .data$y),
      shape = 21, size = 2.2, stroke = 0.5, fill = "white", colour = PAL[["ink"]])} +
    ggplot2::geom_segment(data = tubes,
                          ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
                          colour = PAL[["ink"]], linewidth = 0.45) +
    ggplot2::geom_segment(ggplot2::aes(x = p_cx, y = Y0, xend = p_cx, yend = Y0 - 5),
                          colour = PAL[["ink"]], linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = PAL, guide = "none") +
    ggplot2::geom_text(data = lvl,
                       ggplot2::aes(x = xlim[1] + 0.012 * diff(xlim), y = .data$y,
                                    label = .data$lab),
                       colour = "grey45", size = 3, hjust = 0, vjust = -0.4) +
    # Exact, unexpanded limits and no axis furniture: the panel then fills the
    # whole image, so the HTML +/- controls in app.R can be placed over the
    # figure by a plain linear map from data units to per-cent.
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  # ---- energy scales: P on the left, the lactic column on the right ---------
  # Each reads ENERGY SPENT FROM FULL, 0 at that reservoir's own top. The
  # column drains downward, so eLa's scale runs out before Gl's starts.
  if (isTRUE(ruler)) {
    # `vj` lets a scale push its end label clear of the neighbouring scale's:
    # eLa's last tick and Gl's first sit at exactly the same height (the rim).
    tick <- function(df, side, col) {
      dir <- if (side == "left") -1 else 1
      if (is.null(df$vj)) df$vj <- 0.4
      list(
        ggplot2::geom_segment(
          data = df, ggplot2::aes(x = .data$x0, y = .data$y,
                                  xend = .data$x0 + dir * 2.6, yend = .data$y),
          colour = col, linewidth = 0.4),
        ggplot2::geom_text(
          data = df, ggplot2::aes(x = .data$x0 + dir * 3.6, y = .data$y,
                                  label = format(round(.data$used), trim = TRUE),
                                  vjust = .data$vj),
          colour = col, size = 3.1, hjust = if (side == "left") 1 else 0))
    }

    rl <- hyd_p_ruler(d)
    if (!is.null(rl)) {
      rl$y  <- yl(rl$h)
      rl$x0 <- p_wallL(rl$y) - 1.2
      pl <- pl +
        ggplot2::geom_segment(
          ggplot2::aes(x = p_wallL(yl(0)) - 1.2, y = yl(0),
                       xend = p_wallL(yl(1)) - 1.2, yend = yl(1)),
          colour = "grey70", linewidth = 0.3) +
        tick(rl, "left", "grey40") +
        ggplot2::geom_text(
          ggplot2::aes(x = p_wallL(yl(0)) - 3.6, y = Y0 - 7,
                       label = "J·kg⁻¹ spent"),
          colour = "grey55", size = 2.8, hjust = 1)
    }

    cr <- hyd_col_rulers(d, ela_pow)
    x_sc <- col_xR + 2.5
    for (nm in c("eLa", "Gl")) {
      df <- cr[[nm]]
      if (is.null(df)) next
      df$y <- yl(df$h); df$x0 <- x_sc; df$vj <- 0.4
      # the two scales meet at the Gl rim; part their labels there
      if (nm == "eLa") df$vj[which.max(df$used)] <- 1.25
      if (nm == "Gl")  df$vj[which.min(df$used)] <- -0.45
      pl <- pl +
        ggplot2::geom_segment(
          ggplot2::aes(x = x_sc, y = min(df$y), xend = x_sc, yend = max(df$y)),
          colour = "grey70", linewidth = 0.3) +
        tick(df, "right", PAL[[nm]])
    }
    if (!is.null(cr$Gl))
      pl <- pl + ggplot2::geom_text(
        ggplot2::aes(x = x_sc, y = Y0 - 7, label = "J·kg⁻¹ spent"),
        colour = "grey55", size = 2.8, hjust = 0)
  }

  labs <- data.frame(
    x = c(mean(ox_x), mean(px), os_x0 + os_w/2, col_xR - SCX*d$w_Gl/2,
          col_xR - SCX*d$w_eLa_bot/2),
    y = c(y_L1 + 16, y_top + 2, y_top + 2, yl((d$ell2 + z_mid)/2), yl(z_top) + 4),
    lab = c("Ox", "P", "OS", "Gl", "eLa"),
    col = c(PAL[["Ox"]], PAL[["P"]], PAL[["OS"]], PAL[["Gl"]], PAL[["eLa"]]))
  pl <- pl + ggplot2::geom_text(data = labs, ggplot2::aes(.data$x, .data$y, label = .data$lab),
                                colour = labs$col, size = 4, fontface = "bold")
  if (nrow(H)) {
    pl <- pl +
      ggplot2::geom_point(data = H, ggplot2::aes(.data$x, .data$y), shape = 21,
                          size = 4.4, fill = "white", colour = "grey35", stroke = 0.5) +
      ggplot2::geom_text(data = H, ggplot2::aes(.data$x, .data$y, label = .data$lab),
                         size = 3.1, colour = "grey20")
  }
  attr(pl, "handles") <- H
  attr(pl, "lims") <- list(xlim = xlim, ylim = ylim)
  attr(pl, "anchors") <- L$anchors
  pl
}

# Which design field each handle drives, and by how much per click.
HANDLE_MAP <- list(
  vo2m_up = c("vo2max", 2.0),  vo2m_dn = c("vo2max", -2.0),
  ell1_up = c("ell1",  0.025), ell1_dn = c("ell1", -0.025),
  ell2_up = c("ell2",  0.025), ell2_dn = c("ell2", -0.025),
  wP_up   = c("w_P",   1.0),   wP_dn   = c("w_P",  -1.0),
  wPt_up  = c("w_P_top", 1.0), wPt_dn = c("w_P_top", -1.0),
  wGl_up  = c("w_Gl",  6.0),   wGl_dn  = c("w_Gl", -6.0),
  wGlt_up = c("w_Gl_top", 6.0), wGlt_dn = c("w_Gl_top", -6.0),
  glH_up  = c("gl_h",  0.025), glH_dn  = c("gl_h", -0.025),
  elaH_up = c("ela_h", 0.025), elaH_dn = c("ela_h", -0.025),
  wElab_up = c("w_eLa_bot", 1.0), wElab_dn = c("w_eLa_bot", -1.0),
  wElat_up = c("w_eLa_top", 1.0), wElat_dn = c("w_eLa_top", -1.0),
  wOSb_up = c("w_OS_bot", 0.4), wOSb_dn = c("w_OS_bot", -0.4),
  wOSn_up = c("w_OS_neck", 0.3), wOSn_dn = c("w_OS_neck", -0.3),
  osNh_up = c("os_neck_h", 0.05), osNh_dn = c("os_neck_h", -0.05),
  wOSy_up = c("w_OS_body", 0.6), wOSy_dn = c("w_OS_body", -0.6)
)
