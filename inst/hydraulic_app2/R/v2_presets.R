# =============================================================================
# LOCKED DEFAULTS
#
# Two named configurations, both in code. `chapter` is di Prampero's, unchanged.
# `jeremy` is the set arrived at by fitting the case figures, locked here on
# 19 August 2026 from the configuration that had been saved to disk while it was
# being decided.
#
# This replaces R/v2_prefs.R, which stored ONE configuration in the user's config
# directory. That was the right tool for deciding the defaults and the wrong one
# for shipping them: a hosted app has no persistent per-user config directory,
# and a published figure should not depend on a file that only exists on the
# machine it was tuned on.
#
# A preset states its design as OVERRIDES on design_defaults(), so a design field
# added later is inherited at its reference value rather than silently dropped.
# =============================================================================

#' The panel settings a preset carries, and how each is put back
#'
#' Every input the model reads must appear here; the test
#' "every panel control that reaches the model is in the presets" enforces it.
HY2_PRESET_INPUTS <- c(
  ptype = "radio", sim_gcase = "select", intensity = "slider",
  duration = "num", work = "num", rest = "num", nreps = "num",
  rest_int = "num", warmup = "num", recovery = "num",
  valve = "radio", a_recovery = "num", float_tubes = "check",
  gA_head_ref = "slider", lactic_power = "num", tau_Z = "num", z1_max = "slider",
  ela_pow = "slider", os_pow = "slider", match_curv = "check")

HY2_PRESETS <- list(

  chapter = list(
    label = "di Prampero",
    blurb = paste("The chapter's figure and its reference athlete:",
                  "V̇O₂max 60 mL·kg⁻¹·min⁻¹,",
                  "cylindrical P, rectangular Gl, tubes fixed."),
    # no overrides: design_defaults() IS the chapter
    design = list(),
    inputs = list(
      ptype = "continuous", sim_gcase = "fig5_110", intensity = 105,
      duration = 180, work = 30, rest = 30, nreps = 10, rest_int = 0,
      warmup = 10, recovery = 300, valve = "one_way", a_recovery = 2.5,
      float_tubes = FALSE, gA_head_ref = 0.052, lactic_power = 44.9,
      tau_Z = 0, z1_max = 100, ela_pow = 1, os_pow = 1, match_curv = FALSE)),

  jeremy = list(
    label = "Jeremy's",
    blurb = paste("Adjusted to follow the case figures more closely:",
                  "a bigger, tapering P, a taller Gl body under a short eLa",
                  "wedge, L₁ raised to 0.535, a wider O₂ store, floating",
                  "tubes, an Ox tap that takes 20 s and a store capped at 25%."),
    design = list(
      vo2max    = 66.8899521531101,   # chapter 60
      ell1      = 0.535,              # chapter 0.335
      w_P       = 14,                 # chapter 10
      w_P_top   = 8,                  # chapter 10, ie a cylinder
      gl_h      = 0.535,              # chapter 0.335
      w_Gl      = 77.4728485233407,   # chapter 89.4728485233407
      ela_h     = 0.165,              # chapter 0.665
      w_eLa_bot = 87.3984962406015,   # chapter 9.3984962406015
      w_eLa_top = 72.5,               # chapter 5.6390977443609
      os_neck_h = 0.25,               # chapter 0.3
      w_OS_neck = 1.7,                # chapter 0.9
      w_OS_body = 10.1,               # chapter 8.6
      w_OS_bot  = 1.4),               # chapter 0.9
    inputs = list(
      ptype = "gastin", sim_gcase = "m400", intensity = 100,
      duration = 180, work = 30, rest = 30, nreps = 10, rest_int = 0,
      warmup = 10, recovery = 300, valve = "one_way", a_recovery = 2.5,
      float_tubes = TRUE, gA_head_ref = 0.052, lactic_power = 100,
      tau_Z = 20, z1_max = 25, ela_pow = 1, os_pow = 1, match_curv = FALSE))
)

#' The preset the app opens on
HY2_PRESET_START <- "chapter"

hy2_preset <- function(name = HY2_PRESET_START) {
  if (!is.character(name) || length(name) != 1 || !name %in% names(HY2_PRESETS))
    name <- HY2_PRESET_START
  HY2_PRESETS[[name]]
}

#' A preset's design: its overrides merged onto the chapter's
hy2_preset_design <- function(name = HY2_PRESET_START) {
  d <- design_defaults()
  ov <- hy2_preset(name)$design
  for (nm in intersect(names(ov), names(d))) d[[nm]] <- ov[[nm]]
  d
}

#' Push a preset's panel settings into the widgets
hy2_preset_apply_inputs <- function(session, name = HY2_PRESET_START) {
  inp <- hy2_preset(name)$inputs
  for (id in names(HY2_PRESET_INPUTS)) {
    v <- inp[[id]]
    if (is.null(v)) next
    switch(HY2_PRESET_INPUTS[[id]],
      radio  = shiny::updateRadioButtons(session, id, selected = v),
      select = shiny::updateSelectInput(session, id, selected = v),
      slider = shiny::updateSliderInput(session, id, value = v),
      num    = shiny::updateNumericInput(session, id, value = v),
      check  = shiny::updateCheckboxInput(session, id, value = isTRUE(v)))
  }
  invisible(TRUE)
}

#' Choices for the selector, in display order
hy2_preset_choices <- function() {
  stats::setNames(names(HY2_PRESETS),
                  vapply(HY2_PRESETS, function(p) p$label, character(1)))
}
