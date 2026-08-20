# =============================================================================
# HYDRAULIC SIMULATOR — v2
#
# One page, one job: design the reservoirs on the figure, drive them with an
# exercise, and read what each pathway supplied.
#
# What v2 drops relative to v1, and why:
#   * every tab except the simulator — the case-figure comparison, the floating
#     tube study and the equations page were working notes, not a tool;
#   * the actuator time constants of glycolysis and of the O2 store, the mouth
#     transfer function and the lactate-clearance constants — all were pinned at
#     their defaults in every configuration that survived, so they were choices
#     the panel invited but nobody made;
#   * the "head at which A' reaches A'max" slider, which is inert whenever tube A
#     floats (see the popover) and so is shown only for the fixed tube.
#
# What it adds: a control for Gl's rim width, cumulative pathway energies, and
# head-line numbers that are the time integrals OF THE PLOTTED BANDS rather than
# a separate accounting that could disagree with the picture.
#
# Model code is shared with inst/hydraulic_app (R/ holds links to it), so the two
# apps can never simulate different physics.
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

EQ_O2_APP <- 20.9
EQ_LA_APP <- 3.0

num <- function(id, label, value, min = NA, max = NA, step = NA) {
  numericInput(id, label, value = value, min = min, max = max, step = step,
               width = "100%")
}

# A numeric box that has been cleared reports NA, and NA reaching hyd_defaults()
# trips a stopifnot and takes the whole app down mid-edit. Fall back to the
# default rather than crash while someone is halfway through typing.
nz <- function(x, default) if (is.null(x) || !is.finite(x)) default else x

`%||%` <- function(a, b) if (is.null(a)) b else a


# ============================== UI ==========================================
ui <- page_sidebar(
  window_title = "Hydraulic simulator",
  fillable = FALSE,
  theme = bs_theme(version = 5, preset = "shiny",
                   primary = "#0072B2", success = "#009E73",
                   warning = "#E69F00", danger = "#D55E00"),

  sidebar = sidebar(
    width = 352, class = "hy-rail", bg = HY2_RAIL, fg = "#e6edf5",
    padding = 0, gap = 0,

    tags$div(
      class = "hy-brand",
      tags$div(class = "hy-brand-mark", bsicons::bs_icon("droplet-half")),
      tags$div(
        tags$div(class = "hy-brand-name", "hydraulic simulator"),
        tags$div(class = "hy-brand-sub", "after di Prampero · Chapter I-6, Fig. I-6.1"))
    ),

    tags$div(
      class = "hy-rail-body",

      # ---------------------------- defaults ---------------------------------
      hy2_section("defaults", hy2_info(
        "Which configuration the app starts from",
        tags$p(tags$b("di Prampero"), " is the chapter's own: the reference ",
               "athlete at V̇O₂max 60 mL·kg⁻¹·min⁻¹, cylindrical P, rectangular ",
               "Gl, tubes fixed, no delay on the Ox tap. This is what the app ",
               "opens on, and what every statement in the tutorial about the ",
               "chapter refers to."),
        tags$p(tags$b("Jeremy's"), " is the set arrived at by fitting the case ",
               "figures: a larger P tapering toward its top, a taller Gl body ",
               "under a short eLa wedge, L₁ raised from 0.335 to 0.535, a wider ",
               "O₂ store, floating tubes, an Ox tap that takes 20 s and a store ",
               "capped at 25% of V̇O₂max."),
        tags$p("Choosing one loads its figure and its panel settings together. ",
               "Edit freely afterwards; ", tags$b("Reset figure"), " returns to ",
               "the selected preset."))),
      tags$div(class = "hy-seg",
               radioButtons("preset", NULL, choices = hy2_preset_choices(),
                            selected = HY2_PRESET_START)),
      uiOutput("preset_blurb"),

      # ---------------------------- exercise ---------------------------------
      hy2_section("exercise", hy2_info(
        "The demand U′",
        tags$p("The tap S under P sets the outflow U′ — the metabolic power the ",
               "muscle has to produce. Everything else in the figure is a supply ",
               "answering it."),
        tags$p(tags$b("Continuous"), " and ", tags$b("intermittent"),
               " are square waves at a chosen fraction of V̇O₂max. ",
               tags$b("Case figure"), " instead replays a published effort, and ",
               "draws the partition published with it as dashed lines over the ",
               "simulated one — so the panel can be worked until the two agree."),
        tags$p(tags$b("Gastin (2001)"), " — all-out and constant-load laboratory ",
               "efforts, digitised from the review's Figs. 4 and 5. Demand and ",
               "supplies come from the same figure, so they close exactly."),
        tags$p(tags$b("Briand et al. (2025)"), " — the six Berlin-2009 sprint ",
               "finals. Here the demand is a ", tags$i("measurement"),
               " (di Prampero's metabolic power from the split times) and the ",
               "three dashed curves are a ", tags$i("model of that measurement"),
               ", so they fall about a per cent short of it. The daylight ",
               "between the top dashed line and the demand line is that model's ",
               "own residual, not a fault of the simulation."))),

      tags$div(class = "hy-seg",
               radioButtons("ptype", NULL, selected = "continuous",
                            choices = c("continuous" = "continuous",
                                        "intermittent" = "intermittent",
                                        "case figure" = "gastin"))),

      conditionalPanel(
        "input.ptype == 'gastin'",
        selectInput("sim_gcase", "case", choices = CASE_CHOICES,
                    selected = "fig5_110")),

      conditionalPanel(
        "input.ptype != 'gastin'",
        sliderInput("intensity", "intensity (% of V̇O₂max)",
                    min = 20, max = 400, value = 105, step = 5, post = " %",
                    ticks = FALSE),
        conditionalPanel("input.ptype == 'continuous'",
                         num("duration", "work duration (s)", 180, 1, 7200, 10)),
        conditionalPanel(
          "input.ptype == 'intermittent'",
          layout_columns(col_widths = c(6, 6),
                         num("work", "work (s)", 30, 1, 3600, 5),
                         num("rest", "recovery (s)", 30, 0, 3600, 5)),
          layout_columns(col_widths = c(6, 6),
                         num("nreps", "repetitions", 10, 1, 60, 1),
                         num("rest_int", "recovery (%)", 0, 0, 100, 5))),
        layout_columns(col_widths = c(6, 6),
                       num("warmup", "rest before (s)", 10, 0, 600, 5),
                       num("recovery", "recovery after (s)", 300, 0, 3600, 30))),

      # ------------------------------ model ----------------------------------
      hy2_section("tube A — glycolysis to P", hy2_info(
        "Which way can tube A run?",
        tags$p(tags$b("One-way."), " A valve lets fluid leave the lactic column ",
               "only. Glycogen is not resynthesised from the phosphagen pool, and ",
               "restoring it takes a much longer timescale than the current ",
               "simulation."),
        tags$p(tags$b("Two-way."), " The tube reverses during recovery, so the ",
               "column refills at a rate set below. Production also falls as the ",
               "column empties. This is the reading used by the later modelling ",
               "lineage (Morton, etc.) built on this figure."))),

      tags$div(class = "hy-seg",
               radioButtons("valve", NULL, selected = "one_way",
                            choices = c("one-way valve" = "one_way",
                                        "two-way valve" = "two_way"))),

      conditionalPanel(
        "input.valve == 'two_way'",
        num("a_recovery", "refill divisor  A′max / M_R", 2.5, 1, 10, 0.5)),

      checkboxInput("float_tubes", "floating tube A", FALSE),
      tags$div(class = "hy-note", style = "margin:-.45rem 0 .7rem;",
               "A follows P's surface — the head is the fluid standing above it.",
               hy2_info(
                 "Floating tube A",
                 tags$p("A fixed tube sits at the foot of the vessel and its flow ",
                        "saturates as soon as the head passes a reference value. A ",
                        "floating tube instead rides on P's surface, so the driving ",
                        "head is the whole depth of lactic fluid standing above P:"),
                 tags$div(class = "text-center my-2",
                          tags$b("A′ = A′max · (h_La − h_P)⁺ / column height")),
                 tags$p("At rest nothing stands above P, so A′ = 0 and glycolysis ",
                        "switches on only as P falls: the activation delay is ",
                        "emergent rather than imposed by a time constant. The ",
                        "normalisation is geometric, which is one free parameter ",
                        "fewer — the reference-head slider is inert in this mode ",
                        "and is therefore hidden."))),

      conditionalPanel(
        "!input.float_tubes",
        hy2_label(tags$span("head at which A′ reaches A′max"), hy2_info(
          "Only applies with fixed tubes",
          tags$p("Sets the bore of tube A: A′ = A′max · head / this value, ",
                 "capped at A′max. Measured up the vessel, so 0.05 means the tube ",
                 "is wide open once P has fallen 5% of the vessel height below the ",
                 "lactic column."))),
        sliderInput("gA_head_ref", NULL, min = 0.02, max = 1,
                    value = 0.052, step = 0.002, ticks = FALSE)),

      hy2_label(tags$span("A′max — maximal lactic power (W·kg⁻¹)"), hy2_info(
        "Maximal lactic power",
        tags$p("The maximal rate that can flow through tube A. This rate can only ",
               "be reached if the reservoir is full and tube L₂ is placed at the ",
               "bottom of Gl."),
        tags$p("In the floating-tube scenario the power depends on the amount of ",
               "liquid above the tube, and therefore the maximal lactic power is ",
               "never fully reached in physiological conditions."))),
      num("lactic_power", NULL, 44.9, 5, 120, 1),
      uiOutput("lactic_readout"),

      # ---------------------------- geometry ---------------------------------
      hy2_section("wall curvature", hy2_info(
        "Changing wall shape",
        tags$p("Cross-sectional area is what turns stored energy into fluid ",
               "LEVEL, and level is what drives every tube. Each wall follows ",
               "y = xⁿ with the origin at its own base. The width goes as ",
               "(height)^(1/n). n = 1 is a straight taper; n > 1 changes the width ",
               "fast near the base then flattens; n < 1 does the reverse. Curving ",
               "a wall changes the reservoir's energy, which the read-out under ",
               "the figure tracks."))),

      sliderInput("ela_pow", "eLa wall — n", min = 0.25, max = 5, value = 1,
                  step = 0.25, ticks = FALSE),
      checkboxInput("match_curv", "match the O₂ store to eLa", FALSE),
      tags$div(class = "hy-note", style = "margin:-.45rem 0 .7rem;",
               "Same exponent, both measured from each wall's own floor upward.",
               hy2_info(
                 "Matching, eLa and O₂ store curvatures",
                 tags$p("The two walls are given the same exponent AND the same ",
                        "reference end."))),
      conditionalPanel("!input.match_curv",
                       sliderInput("os_pow", "O₂ store wall — n", min = 0.25,
                                   max = 5, value = 1, step = 0.25, ticks = FALSE)),

      # ---------------------------- kinetics ---------------------------------
      hy2_section("kinetics", hy2_info(
        "The one time constant left",
        tags$p("τ is the response time of the Ox tap: the flow delivered chases ",
               "its head-determined target with dZ′/dt = (Z′ss − Z′)/τ. At τ = 0 ",
               "the tap is instantaneous and the only lag in the aerobic response ",
               "is P's own capacitance."),
        tags$p("The equivalent constants for glycolysis and for the O₂ store are ",
               "held at 0. Glycolysis needs no imposed delay once tube A floats — ",
               "it gets one from the geometry — and the store has no enzymatic ",
               "activation to delay: its discharge is set by the dissociation ",
               "curve and by diffusion, both fast."))),
      num("tau_Z", "τ Ox tap (s)", 0, 0, 40, 1),

      hy2_label(tags$span("Z₁ maximal rate (% of V̇O₂max)"), hy2_info(
        "How fast the O₂ store can give up its content",
        tags$p("OS is oxygen already present when exercise starts, held on ",
               "haemoglobin in the venous blood and on myoglobin in the muscle. ",
               "It supplies the muscle before the O₂ cascade can take the relay ",
               "and deliver oxygen continuously. The chapter sizes it from the ",
               "venous O₂ content falling 150 to 50 mL·L⁻¹, about 400 mL."),
        tags$p("The chapter gives Z₁ the same hydraulic resistance as Z, so the ",
               "store may discharge as fast as V̇O₂max. Two things work against ",
               "that. The dissociation curve releases the bound O₂ only as PO₂ ",
               "falls, so the highest rates arrive when the store is already ",
               "low. And the mitochondria cannot take up that oxygen instantly."),
        tags$p("The first is already in the figure, as the narrow neck at the ",
               "top of OS. This slider caps Z₁ outright, as a fraction of ",
               "Z′max. 100% is the chapter."),
        tags$p("It changes the store's timing, not the pathway split: at 200% ",
               "V̇O₂max with τ Ox 20 s, 100% to 10% takes peak Z₁ from 8.8 to ",
               "2.1 W·kg⁻¹ and half-emptying from 9.2 to 22 s, for 2 J·kg⁻¹ of ",
               "lactic energy. It can only cap below what the head and the ",
               "joint ceiling Z + Z₁ ≤ Z′max already allow, so at τ Ox 0 there ",
               "is little left for it to bind on."))),
      sliderInput("z1_max", NULL, min = 0, max = 100, value = 100, step = 5,
                  post = " %", ticks = FALSE),
      tags$div(
        class = "hy-run",
        actionButton("run", "Run simulation", class = "btn-primary",
                     icon = icon("play")),
        uiOutput("run_stale"),
        actionButton("reset", "Reset everything", class = "btn btn-ghost"))
    )
  ),

  # ------------------------------- main ------------------------------------
  hy2_css(),

  navset_underline(
    id = "main_tab",

    nav_panel(
    title = tagList(bsicons::bs_icon("droplet-half"), " simulator"),
    value = "sim",

  card(
    full_screen = TRUE,
    hy2_card_head("sliders", "hydraulic configuration",
                  uiOutput("pending_badge", inline = TRUE)),
    card_body(
      padding = 8,
      tags$div(
        class = "hy-note mb-2",
        "Size each reservoir on the figure — its drawn AREA is the energy it holds."),
      tags$div(
        style = "position:relative;",
        plotOutput("design_plot", height = "460px"),
        tags$div(style = "position:absolute; inset:0; pointer-events:none;",
                 uiOutput("design_overlay"))),
      uiOutput("ref_boxes"),
      tags$div(
        class = "d-flex align-items-center gap-2 mt-3 flex-wrap",
        actionButton("set_cfg", "Apply configuration",
                     class = "btn hy-pill hy-pill-accent", icon = icon("check")),
        actionButton("reset_design", "Reset figure", class = "btn hy-pill"))
    )
  ),

  card(
    full_screen = TRUE,
    hy2_card_head("graph-up", "energy supply",
                  downloadButton("dl", "export CSV", class = "btn hy-pill",
                                 icon = icon("download"))),
    card_body(
      padding = 8,
      plotOutput("p_power", height = "420px"),
      tags$div(
        class = "hy-transport",
        actionButton("play", "Play", icon = icon("play"),
                     class = "btn hy-pill", width = "112px"),
        tags$div(class = "hy-clock", textOutput("clock", inline = TRUE)),
        tags$div(style = "min-width:132px;",
                 selectInput("speed", NULL, selected = 30, width = "132px",
                             choices = c("5 s / s" = 5, "15 s / s" = 15,
                                         "30 s / s" = 30, "60 s / s" = 60))),
        tags$div(class = "flex-grow-1",
                 sliderInput("tsel", NULL, min = 0, max = 100, value = 0,
                             step = 1, width = "100%", post = " s")))
    )
  ),

  # The head-line numbers sit UNDER the figure they are the integrals of, so the
  # eye goes picture first, totals second, rather than meeting four numbers with
  # no context at the top of the page.
  uiOutput("metrics"),

  card(
    full_screen = TRUE,
    hy2_card_head("graph-up-arrow", "energy expended"),
    card_body(
      padding = 8,
      tags$div(class = "hy-note mb-1",
               "The running time integral of each pathway — read the end of a ",
               "curve for that pathway's total."),
      plotOutput("p_cum", height = "420px"))
  ),

  card(
    full_screen = TRUE,
    hy2_card_head("activity", "pathway power"),
    card_body(
      padding = 8,
      tags$div(class = "hy-note mb-1",
               "The same three supplies as above, each from a common zero rather ",
               "than stacked, so peaks and decay rates can be read directly."),
      plotOutput("p_lines", height = "420px"))
  )
    ),

    # ----------------------------- the tour ---------------------------------
    nav_panel(
      title = tagList(bsicons::bs_icon("mortarboard"), " tutorial"),
      value = "tour",
      tags$div(
        class = "hy-tour",
        tags$aside(class = "hy-tour-nav", uiOutput("tour_nav")),
        tags$div(
          class = "hy-tour-main",
          tags$div(class = "hy-tour-head",
                   tags$div(class = "hy-tour-step", textOutput("tour_counter", inline = TRUE)),
                   tags$h2(class = "hy-tour-title", textOutput("tour_title", inline = TRUE))),
          tags$div(class = "hy-tour-body", uiOutput("tour_body")),
          # STATIC footer: a renderUI would recreate these buttons on every step
          # and Shiny resets an action button's counter when it is recreated,
          # which fires the observer and jumps the step on its own.
          tags$div(
            class = "hy-tour-foot",
            actionButton("tour_prev", "Back", class = "btn hy-pill",
                         icon = icon("chevron-left")),
            tags$div(class = "hy-tour-dots", uiOutput("tour_dots", inline = TRUE)),
            actionButton("tour_next", "Next", class = "btn hy-pill hy-pill-accent",
                         icon = icon("chevron-right")))))
    )
  )
)


# ============================ SERVER ========================================
server <- function(input, output, session) {

  # Open on the chapter's configuration. Both presets are in code (R/v2_presets.R),
  # so what a colleague opens is what this repository says, not a file that
  # happens to exist on one machine.
  start_d <- hy2_preset_design(HY2_PRESET_START)
  design  <- reactiveVal(start_d)
  applied <- reactiveVal(start_d)

  # `preset` names the configuration the figure was last loaded FROM, which is
  # what Reset figure and the tour experiments build on. It is not a claim that
  # the panel still matches it: editing is the point.
  cur_preset <- reactive(if (is.null(input$preset)) HY2_PRESET_START else input$preset)

  # A tour experiment may switch the preset itself, and then set inputs of its
  # own on top. Updating the radio is a client round-trip, so this observer would
  # fire afterwards and undo them; the flag lets the loader own that one change.
  preset_from_lab <- reactiveVal(FALSE)

  observeEvent(input$preset, {
    if (isTRUE(preset_from_lab())) { preset_from_lab(FALSE); return(invisible(NULL)) }
    d <- hy2_preset_design(input$preset)
    design(d); applied(d)
    hy2_preset_apply_inputs(session, input$preset)
  }, ignoreInit = TRUE)

  output$preset_blurb <- renderUI(
    tags$p(class = "hy-note mb-0", hy2_preset(cur_preset())$blurb))

  # ---- the schematic as a control surface -----------------------------------
  output$design_overlay <- renderUI({
    L   <- hyd_design_layout(design())
    A   <- L$anchors
    xp  <- 100 * (A$x - L$xlim[1]) / diff(L$xlim)
    yp  <- 100 * (1 - (A$y - L$ylim[1]) / diff(L$ylim))
    tagList(lapply(seq_len(nrow(A)), function(i) tags$div(
      class = "hy-nudge",
      style = sprintf("left:%.2f%%; top:%.2f%%; border-color:%s;", xp[i], yp[i], A$col[i]),
      tags$div(class = "hy-nudge-lab", style = sprintf("color:%s;", A$col[i]), A$lab[i]),
      tags$div(class = "btn-group btn-group-sm",
               actionButton(paste0(A$id[i], "_dn"), "−", class = "btn btn-outline-secondary"),
               actionButton(paste0(A$id[i], "_up"), "+", class = "btn btn-outline-secondary"))
    )))
  })

  lapply(names(HANDLE_MAP), function(hid) {
    observeEvent(input[[hid]], {
      m <- HANDLE_MAP[[hid]]
      design(design_nudge(design(), m[1], as.numeric(m[2])))
    }, ignoreInit = TRUE)
  })

  # Curvature lives on the DESIGN, so the drawing, the reference read-out and the
  # model can never disagree about which wall is being integrated.
  observeEvent(list(input$ela_pow, input$os_pow, input$match_curv), {
    d <- isolate(design())
    d$ela_pow      <- input$ela_pow
    d$os_pow       <- if (isTRUE(input$match_curv)) input$ela_pow else input$os_pow
    d$os_from_base <- isTRUE(input$match_curv)
    design(d); applied(d)
  }, ignoreInit = TRUE)

  observeEvent(input$reset_design, design(hy2_preset_design(cur_preset())))

  # ---- (configuration presets live in R/v2_presets.R) -----------------------
  observeEvent(input$set_cfg, {
    applied(design())
    showNotification("Configuration applied — press Run simulation.",
                     type = "message", duration = 3)
  })

  # Edits on the figure reach the model only through Apply. Without a cue, an
  # edited-but-unapplied geometry looks like "changing the reservoir does nothing".
  output$pending_badge <- renderUI({
    if (identical(design(), applied())) return(NULL)
    tags$span(class = "hy-badge-warn", "not applied")
  })

  # `animating` is FALSE until Play is pressed or the scrubber is moved. A run
  # therefore leaves the reservoirs FULL: parking the figure at the end of the
  # run showed drained reservoirs next to a design you were about to edit, which
  # reads as the design rather than as the last frame of a simulation.
  animating <- reactiveVal(FALSE)

  output$design_plot <- renderPlot({
    d   <- design()
    row <- if (!isTRUE(animating())) NULL else tryCatch({
      s <- sim(); s$d[which.min(abs(s$d$time - tnow())), , drop = FALSE]
    }, error = function(e) NULL)
    hyd_schematic_design(d, row = row, handles = FALSE,
                         float = isTRUE(input$float_tubes), ruler = TRUE)
  }, res = 100)

  output$lactic_readout <- renderUI({
    Ap <- input$lactic_power
    if (is.null(Ap) || is.na(Ap)) return(NULL)
    Z  <- design_capacities(applied())$Zmax
    r  <- function(lab, val) tags$div(class = "r", tags$span(lab), tags$b(val))
    tags$div(class = "hy-readout",
             r("O₂ equivalent", sprintf("%.0f mL·kg⁻¹·min⁻¹", Ap / EQ_O2_APP * 60)),
             r("× maximal aerobic power", sprintf("%.2f", Ap / Z)),
             r("vLamax", sprintf("%.2f mmol·L⁻¹·s⁻¹", Ap / 62)))
  })

  # ---- adjusted reservoirs against the reference athlete --------------------
  output$ref_boxes <- renderUI({
    cmp  <- design_vs_reference(design())
    cols <- c("#0072B2", "#009E73", "#D55E00", "#CC79A7", "#B22222", "#E69F00")
    boxes <- lapply(seq_len(nrow(cmp)), function(i) {
      r   <- cmp[i, ]
      off <- abs(r$pct - 100)
      tags$div(
        class = "hy-ref", style = sprintf("border-color:%s;", cols[i]),
        tags$div(class = "hy-ref-name", style = sprintf("color:%s;", cols[i]), r$quantity),
        tags$div(class = "hy-ref-val", sprintf(paste0("%.", r$digits, "f"), r$value)),
        tags$div(class = "hy-ref-unit", r$unit),
        tags$div(class = "hy-ref-badge",
                 style = sprintf("color:%s;", if (off < 2) "#9aa5b1" else cols[i]),
                 if (off < 2) "at reference" else sprintf("%+.0f%%", r$pct - 100)))
    })
    tags$div(
      class = "mt-2",
      tags$div(class = "hy-note mb-1",
               "Adjusted reservoirs against the reference athlete."),
      tags$div(class = "hy-refs", boxes))
  })

  # ---- parameters -----------------------------------------------------------
  # NOTE every geometric parameter comes from the design; only the things the
  # rail controls are passed here. Passing a field design_to_params() already
  # sets would be a duplicated formal argument and would kill the app at load.
  params <- reactive({
    design_to_params(
      applied(),
      gl_refill    = identical(input$valve, "two_way"),
      a_recovery   = nz(input$a_recovery, 2.5),
      lactic_power = nz(input$lactic_power, 44.9),
      gA_head_ref  = nz(input$gA_head_ref, 0.052),
      tau_Z        = nz(input$tau_Z, 0),
      z1_max_frac  = nz(input$z1_max, 100) / 100,
      tau_A        = 0,
      tau_Z1       = 0,
      float_tubes  = isTRUE(input$float_tubes)
    )
  })

  # A case figure dictates the athlete, so its V̇O₂max goes into the DESIGN — not
  # into a local copy inside params(), which would leave the figure, the V̇O₂max
  # control and the reference read-out all showing the previous value. The old
  # value is restored on leaving the case figures.
  gc_prev_vo2max <- reactiveVal(NULL)
  observeEvent(list(input$ptype, input$sim_gcase), {
    d <- isolate(design())
    if (identical(input$ptype, "gastin") && !is.null(input$sim_gcase)) {
      if (is.null(gc_prev_vo2max())) gc_prev_vo2max(d$vo2max)
      d$vo2max <- case_vo2max(input$sim_gcase)
    } else if (!is.null(gc_prev_vo2max())) {
      d$vo2max <- gc_prev_vo2max(); gc_prev_vo2max(NULL)
    } else return()
    design(d); applied(d)
  }, ignoreInit = TRUE)

  sim_case <- reactive({
    if (!identical(input$ptype, "gastin")) return(NULL)
    req(input$sim_gcase)
    case_data(input$sim_gcase)
  })

  protocol <- reactive({
    g <- sim_case()
    if (!is.null(g))
      return(list(time = g$time, power = g$demand * EQ_O2_APP / 60, marks = numeric(0)))
    hyd_protocol(
      type = input$ptype, intensity = nz(input$intensity, 105) / 100,
      duration = nz(input$duration, 180), work = nz(input$work, 30),
      rest = nz(input$rest, 30), n_reps = nz(input$nreps, 10),
      rest_intensity = nz(input$rest_int, 0) / 100,
      recovery = nz(input$recovery, 300), warmup = nz(input$warmup, 10),
      p = params())
  })

  sim <- eventReactive(input$run, ignoreNULL = FALSE, {
    p  <- params()
    pr <- protocol()
    withProgress(message = "Integrating…", value = 0.4, {
      d <- hyd2_attach_cum(hyd_simulate(pr$time, pr$power, p))
      incProgress(0.6)
      list(d = d, p = p, pr = pr)
    })
  })

  # Nothing on screen distinguishes "this is the run you asked for" from "you
  # changed four things and the figures are still the previous run". Every input
  # that reaches the model is folded into one key; when it moves away from the
  # key the last run used, say so under the button.
  settings_key <- reactive({
    paste(input$ptype, input$sim_gcase, input$intensity, input$duration,
          input$work, input$rest, input$nreps, input$rest_int, input$warmup,
          input$recovery, input$valve, input$a_recovery, input$float_tubes,
          input$gA_head_ref, input$lactic_power, input$tau_Z, input$z1_max,
          paste(unlist(applied()), collapse = ","), sep = "|")
  })
  ran_key <- reactiveVal(NULL)
  observeEvent(sim(), ran_key(isolate(settings_key())))
  output$run_stale <- renderUI({
    if (identical(settings_key(), ran_key())) return(NULL)
    tags$div(class = "hy-stale", tags$span(class = "hy-stale-dot"),
             "settings changed since this run")
  })

  # ---- playback -------------------------------------------------------------
  FPS      <- 10
  tnow     <- reactiveVal(0)
  playing  <- reactiveVal(FALSE)
  scrubbed <- reactiveVal(0)

  # A finished run shows its WHOLE self. Parking the clock at 0 left the two
  # figures blank until Play was pressed, which reads as a broken simulation
  # rather than as frame one of an animation. Play rewinds and replays.
  observeEvent(sim(), {
    tmax <- ceiling(max(sim()$d$time))
    tnow(tmax); playing(FALSE); scrubbed(tmax); animating(FALSE)
    updateSliderInput(session, "tsel", min = 0, max = tmax, value = tmax,
                      step = max(round(tmax / 400, 1), 0.1))
    updateActionButton(session, "play", label = "Play", icon = icon("play"))
  })

  observeEvent(input$play, {
    playing(!isTRUE(playing()))
    if (isTRUE(playing())) {
      if (tnow() >= max(sim()$d$time) - 1e-9) tnow(0)
      animating(TRUE)
      updateActionButton(session, "play", label = "Pause", icon = icon("pause"))
    } else {
      updateActionButton(session, "play", label = "Play", icon = icon("play"))
      updateSliderInput(session, "tsel", value = round(tnow(), 1))
    }
  })

  observeEvent(input$tsel, {
    if (isTRUE(all.equal(input$tsel, scrubbed()))) return()
    scrubbed(input$tsel); playing(FALSE); animating(TRUE)
    updateActionButton(session, "play", label = "Play", icon = icon("play"))
    tnow(input$tsel)
  }, ignoreInit = TRUE)

  observe({
    if (!isTRUE(playing())) return()
    invalidateLater(round(1000 / FPS))
    isolate({
      tmax <- max(sim()$d$time)
      nxt  <- tnow() + as.numeric(input$speed) / FPS
      if (nxt >= tmax) nxt <- 0
      tnow(nxt)
    })
  })

  observeEvent(input$reset, {
    updateRadioButtons(session, "ptype", selected = "continuous")
    updateRadioButtons(session, "valve", selected = "one_way")
    updateSliderInput(session, "intensity", value = 105)
    updateNumericInput(session, "duration", value = 180)
    updateNumericInput(session, "lactic_power", value = 44.9)
    updateNumericInput(session, "tau_Z", value = 0)
    updateSliderInput(session, "z1_max", value = 100)
    updateCheckboxInput(session, "float_tubes", value = FALSE)
    updateCheckboxInput(session, "match_curv", value = FALSE)
    updateSliderInput(session, "ela_pow", value = 1)
    updateSliderInput(session, "os_pow", value = 1)
    d0 <- hy2_preset_design(cur_preset())
    design(d0); applied(d0)
    hy2_preset_apply_inputs(session, cur_preset())
  })

  # ---- head-line numbers ----------------------------------------------------
  # Every one of these is read off the SAME three bands the figures draw.
  energy <- reactive(hyd2_energies(sim()$d))

  # The numbers on screen belong to a run, and the panel that produced it may
  # already have moved on. Naming the run above them is the difference between a
  # dashboard and four numbers of unknown provenance.
  output$run_caption <- renderUI({
    s <- sim(); p <- s$p
    what <- if (identical(isolate(input$ptype), "gastin")) {
      sprintf("case figure · %s", case_label(isolate(input$sim_gcase)))
    } else if (identical(isolate(input$ptype), "intermittent")) {
      sprintf("intermittent · %g × %g s at %g%% / %g s recovery",
              nz(isolate(input$nreps), 10), nz(isolate(input$work), 30),
              nz(isolate(input$intensity), 105), nz(isolate(input$rest), 30))
    } else {
      sprintf("continuous · %g s at %g%% of V̇O₂max",
              nz(isolate(input$duration), 180), nz(isolate(input$intensity), 105))
    }
    tags$div(
      class = "hy-metrics-head",
      tags$span(class = "hy-run-name", what),
      tags$span(class = "hy-run-meta", sprintf(
        "V̇O₂max %.1f · A′max %.1f W·kg⁻¹ · %s tube A · %.0f s simulated",
        p$VO2max, p$Amax,
        if (isTRUE(p$float_tubes)) "floating" else "fixed",
        max(s$d$time))))
  })

  output$metrics <- renderUI({
    tags$div(
      class = "hy-metrics-card",
      uiOutput("run_caption"),
      tags$div(
        class = "hy-metrics",
        hy2_metric("peak aerobic power", "mL·kg⁻¹·min⁻¹ above rest",
                   "m_aer", "m_aer_sub", HY2_PAL[["Aerobic"]]),
        hy2_metric("lactic energy", "J·kg⁻¹", "m_lac", "m_lac_sub", HY2_PAL[["Lactic"]]),
        hy2_metric("alactic energy", "J·kg⁻¹", "m_ala", "m_ala_sub", HY2_PAL[["Alactic"]]),
        # "% of total" alone is a trap: the totals run to the END of the
        # simulation, recovery included, and the aerobic system does a lot of
        # work after the tap closes. A 10 s all-out is 25/33/43 over the bout
        # but 47/23/30 once 240 s of recovery is counted. Say which one this is.
        hy2_metric("aerobic / lactic / alactic", "% of total, recovery included",
                   "m_split", "m_split_sub", "#4c5a67")))
  })

  output$m_aer <- renderText({
    pk <- max(sim()$d$vo2_muscle_W, na.rm = TRUE)
    sprintf("%.1f", pk * 60 / sim()$p$EQ_O2)
  })
  output$m_aer_sub <- renderUI({
    s  <- sim(); pk <- max(s$d$vo2_muscle_W, na.rm = TRUE)
    HTML(sprintf("<b>%.0f%%</b> of V̇O₂max · <b>%.1f</b> W·kg⁻¹",
                 100 * pk / s$p$Zmax, pk))
  })

  output$m_lac <- renderText(sprintf("%.0f", energy()$lactic))
  output$m_lac_sub <- renderUI({
    e <- energy()
    HTML(sprintf(
      "peak A′ <b>%.1f</b> W·kg⁻¹<br><b>%.0f%%</b> of total · Δ[La]<sub>b</sub> <b>%.1f</b> mmol·L⁻¹",
      max(sim()$d$Ap, na.rm = TRUE), e$share[["lactic"]],
      e$lactic / EQ_LA_APP / EQ_O2_APP))
  })

  output$m_ala <- renderText(sprintf("%.0f", energy()$alactic))
  output$m_ala_sub <- renderUI({
    e <- energy()
    HTML(sprintf("<b>%.0f%%</b> of total · PCr down to <b>%.0f%%</b> of rest",
                 e$share[["alactic"]], 100 * min(sim()$d$h_P, na.rm = TRUE)))
  })

  output$m_split <- renderText({
    s <- energy()$share
    if (any(is.na(s))) return("—")
    sprintf("%.0f / %.0f / %.0f", s[["aerobic"]], s[["lactic"]], s[["alactic"]])
  })
  # The shares are of what was SUPPLIED. If P ran dry and the inflows could not
  # cover U', that shortfall is not a pathway and is reported beside them.
  output$m_split_sub <- renderUI({
    e <- energy()
    HTML(sprintf("total <b>%.0f</b> J·kg⁻¹%s", e$total,
                 if (e$unmet > 0.5)
                   sprintf(" · <b>%.0f</b> unmet", e$unmet) else ""))
  })

  output$clock <- renderText(sprintf("t = %.0f s", tnow()))

  # ---- figures --------------------------------------------------------------
  # ~200 points keeps a playback frame near 70 ms. The cumulative columns were
  # integrated at full resolution before this, so down-sampling cannot move them.
  plotdat <- reactive({
    d  <- sim()$d
    k  <- unique(round(seq(1, nrow(d), length.out = min(nrow(d), 200))))
    ds <- d[k, , drop = FALSE]
    b  <- hyd2_bands(ds)
    cm <- hyd2_cum(d)
    list(d = ds,
         ymax = max(c(ds$U, b$aerobic + b$lactic + b$alactic), na.rm = TRUE) * 1.06,
         # the unstacked figure needs the tallest SINGLE series, not the stack
         lmax = max(c(ds$U, b$aerobic, b$lactic, b$alactic), na.rm = TRUE) * 1.06,
         cmax = max(c(d$cum2_demand, cm$aerobic, cm$lactic, cm$alactic),
                    na.rm = TRUE) * 1.06)
  })

  output$p_power <- renderPlot({
    pd <- plotdat()
    hyd2_power_plot(pd$d, tnow(), ymax = pd$ymax, reference = isolate(sim_case()))
  }, res = 100)

  output$p_cum <- renderPlot({
    pd <- plotdat()
    hyd2_cum_plot(pd$d, tnow(), ymax = pd$cmax)
  }, res = 100)

  # The two figures above follow the clock frame by frame; this one is a full
  # ggplot rebuild over the whole run, so its marker reads a debounced clock and
  # scrubbing stays smooth.
  tsel_slow <- debounce(reactive(tnow()), 450)
  output$p_lines <- renderPlot({
    pd <- plotdat()
    hyd2_power_lines_plot(pd$d, tnow = tsel_slow(), ymax = pd$lmax)
  }, res = 100)

  # =========================== the tour =====================================
  # Step state. The BODY is rendered dynamically; the prev/next buttons are
  # static in the UI, because Shiny resets an action button's counter when
  # renderUI recreates it, which fires the observer and moves the step by itself.
  # The nav links and the "load these settings" buttons ARE recreated, so each of
  # their observers guards on a non-zero counter for the same reason.
  N_TOUR <- hy2_tour_n()
  tour_i <- reactiveVal(1L)

  observeEvent(input$tour_prev, tour_i(max(1L, tour_i() - 1L)), ignoreInit = TRUE)
  observeEvent(input$tour_next, tour_i(min(N_TOUR, tour_i() + 1L)), ignoreInit = TRUE)

  lapply(seq_len(N_TOUR), function(i) {
    observeEvent(input[[paste0("tour_go_", i)]], {
      req(input[[paste0("tour_go_", i)]] > 0)
      tour_i(i)
    }, ignoreInit = TRUE)
  })

  output$tour_counter <- renderText(sprintf("Step %d of %d", tour_i(), N_TOUR))
  output$tour_title   <- renderText(HY2_TOUR[[tour_i()]]$title)
  output$tour_body    <- renderUI(HY2_TOUR[[tour_i()]]$body())

  output$tour_nav <- renderUI({
    cur <- tour_i()
    tagList(
      tags$div(class = "hy-tour-navhead", "contents"),
      lapply(seq_len(N_TOUR), function(i) {
        st <- HY2_TOUR[[i]]
        actionLink(
          paste0("tour_go_", i),
          tagList(tags$span(class = "hy-tour-navnum", i),
                  tags$span(class = "hy-tour-navtext", st$nav)),
          class = paste("hy-tour-navlink", if (i == cur) "is-active" else ""))
      }))
  })

  output$tour_dots <- renderUI({
    cur <- tour_i()
    tagList(lapply(seq_len(N_TOUR), function(i)
      tags$span(class = paste("hy-dot", if (i == cur) "is-on" else ""))))
  })

  # One canned run behind every figure on the tour, so the pictures a beginner
  # is taught to read are drawn by the same code as the real ones.
  tour_run <- reactive({
    d  <- design_defaults()
    p  <- design_to_params(d, lactic_power = 44.9, tau_Z = 0, tau_A = 0,
                           tau_Z1 = 0, float_tubes = FALSE)
    pr <- hyd_protocol("continuous", intensity = 1.05, duration = 180,
                       recovery = 300, warmup = 10, p = p)
    dd <- hyd2_attach_cum(hyd_simulate(pr$time, pr$power, p))
    k  <- unique(round(seq(1, nrow(dd), length.out = min(nrow(dd), 200))))
    list(d = dd[k, , drop = FALSE], full = dd, p = p, design = d)
  })

  output$tour_schematic <- renderPlot(
    hyd_schematic_design(design_defaults(), ruler = TRUE), res = 100)

  # rest vs mid-exercise, the same figure at two instants: this is the step that
  # has to land, so it is shown rather than asserted
  tour_at <- function(t) {
    r <- tour_run()
    row <- r$full[which.min(abs(r$full$time - t)), , drop = FALSE]
    hyd_schematic_design(r$design, row = row, ruler = TRUE)
  }
  output$tour_rest <- renderPlot(tour_at(0), res = 96)
  output$tour_work <- renderPlot(tour_at(30), res = 96)

  output$tour_shape_cyl <- renderPlot({
    hyd_schematic_design(design_defaults(), ruler = TRUE)
  }, res = 96)
  output$tour_shape_cone <- renderPlot({
    d <- design_defaults(); d$w_P_top <- d$w_P * 0.3
    hyd_schematic_design(d, ruler = TRUE)
  }, res = 96)

  # fixed vs floating tube A: the same run, the same instant, the two tube laws
  tour_tube <- reactive({
    d <- design_defaults()
    mk <- function(float) {
      p  <- design_to_params(d, lactic_power = 44.9, tau_Z = 0, tau_A = 0,
                             tau_Z1 = 0, float_tubes = float)
      pr <- hyd_protocol("continuous", intensity = 1.5, duration = 60,
                         recovery = 120, warmup = 10, p = p)
      hyd_simulate(pr$time, pr$power, p)
    }
    list(design = d, fixed = mk(FALSE), float = mk(TRUE))
  })

  output$tour_tube_fixed <- renderPlot({
    r <- tour_tube()
    hyd_schematic_design(r$design, float = FALSE,
                         row = r$fixed[which.min(abs(r$fixed$time - 30)), , drop = FALSE])
  }, res = 96)
  output$tour_tube_float <- renderPlot({
    r <- tour_tube()
    hyd_schematic_design(r$design, float = TRUE,
                         row = r$float[which.min(abs(r$float$time - 30)), , drop = FALSE])
  }, res = 96)

  output$tour_tube_flow <- renderPlot({
    r  <- tour_tube()
    df <- rbind(
      data.frame(time = r$fixed$time, y = pmax(r$fixed$Ap, 0), who = "fixed at L₂"),
      data.frame(time = r$float$time, y = pmax(r$float$Ap, 0), who = "floating on P"))
    ggplot(df, aes(time, y, colour = who)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = c("fixed at L₂" = "#7b8794",
                                     "floating on P" = HY2_PAL[["Lactic"]])) +
      coord_cartesian(xlim = c(0, 110), expand = FALSE) +
      labs(x = "time  (s)", y = "A′  (W·kg⁻¹)", colour = NULL) +
      theme_hy2()
  }, res = 100)

  output$tour_supply <- renderPlot({
    r <- tour_run()
    hyd2_power_plot(r$d, max(r$d$time))
  }, res = 100)
  output$tour_cum <- renderPlot({
    r <- tour_run(); hyd2_cum_plot(r$d, max(r$d$time))
  }, res = 100)
  output$tour_lines <- renderPlot({
    r <- tour_run(); hyd2_power_lines_plot(r$d)
  }, res = 100)

  # ---- the guided experiments ----------------------------------------------
  # Settings are LOADED, not run. Updating an input is a client round-trip, so
  # firing the simulation from here would run it against the previous values;
  # and pressing Run is the habit the tour is trying to build anyway.
  lapply(names(HY2_TOUR_LABS), function(key) {
    observeEvent(input[[paste0("tour_load_", key)]], {
      req(input[[paste0("tour_load_", key)]] > 0)
      L <- HY2_TOUR_LABS[[key]]

      # Build on the SELECTED preset, not on design_defaults(): an experiment
      # that sets no design of its own must leave the chosen athlete alone, the
      # same way Reset figure does.
      base <- if (!is.null(L$preset)) L$preset else cur_preset()
      if (!identical(base, cur_preset())) {
        preset_from_lab(TRUE)
        updateRadioButtons(session, "preset", selected = base)
        hy2_preset_apply_inputs(session, base)
      }
      d <- hy2_preset_design(base)
      for (nm in names(L$design)) d[[nm]] <- L$design[[nm]]
      design(d); applied(d)

      inp <- L$inputs
      updateRadioButtons(session, "ptype", selected = inp$ptype %||% "continuous")
      updateRadioButtons(session, "valve", selected = inp$valve %||% "one_way")
      updateCheckboxInput(session, "float_tubes", value = isTRUE(inp$float_tubes))
      updateCheckboxInput(session, "match_curv", value = FALSE)
      updateSliderInput(session, "ela_pow", value = 1)
      updateSliderInput(session, "os_pow", value = 1)
      updateNumericInput(session, "tau_Z", value = inp$tau_Z %||% 0)
      updateNumericInput(session, "lactic_power", value = inp$lactic_power %||% 44.9)
      if (!is.null(inp$intensity)) updateSliderInput(session, "intensity", value = inp$intensity)
      for (nm in c("duration", "work", "rest", "nreps", "rest_int", "warmup", "recovery"))
        if (!is.null(inp[[nm]])) updateNumericInput(session, nm, value = inp[[nm]])

      nav_select("main_tab", "sim", session = session)
      showNotification(
        tagList(tags$b(L$label), " loaded — press ", tags$b("Run simulation"),
                " in the panel on the left."),
        type = "message", duration = 7)
    }, ignoreInit = TRUE)
  })

  output$dl <- downloadHandler(
    filename = function() sprintf("hydraulic_%s.csv", input$ptype),
    content  = function(file) utils::write.csv(sim()$d, file, row.names = FALSE)
  )
}

shinyApp(ui, server)
