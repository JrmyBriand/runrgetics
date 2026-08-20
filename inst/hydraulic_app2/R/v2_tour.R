# =============================================================================
# THE TOUR
#
# Pitched at di Prampero's Chapter I-6 and using its terminology: reservoirs and
# tubes, capacity and resistance, flow driven by a level difference, U' as the
# outflow set by the tap S, Z'max as the hydraulic analogue of maximal O2 uptake,
# O2 deficit and debt. Where the chapter gives a number -- venous O2 content, the
# third of muscle glycogen reachable anaerobically, the 10-35 s of maximal O2
# uptake after supramaximal work -- the tour gives the same one.
#
# It is a tour of an app, so it also says where to click, with real screenshots
# of the running app annotated from the same DOM they were captured from (see
# make_tour_shots.R and the generated R/v2_tour_marks.R).
#
# Steps are DATA; `body` is a function so a step can hold plot outputs and
# buttons. Keep it terse: the chapter is terse.
# =============================================================================

HY2_TOUR_LABS <- list(
  sprint = list(
    label = "10 s all-out",
    inputs = list(ptype = "continuous", intensity = 300, duration = 10,
                  warmup = 10, recovery = 240),
    expect = paste("U′ exceeds Z′max + A′max, so P falls past L₂ and cannot",
                   "stabilise. The split reads about 47/23/30, but that counts",
                   "the 4 min of recovery; over the 10 s of work it is 25/33/43.",
                   "Set the recovery to 0 to see the difference.")),
  steady = list(
    label = "10 min at 80%",
    inputs = list(ptype = "continuous", intensity = 80, duration = 600,
                  warmup = 10, recovery = 240),
    expect = paste("U′ < Z′max, so Z′ rises to meet it and P settles between L₀",
                   "and L₁. About 97% aerobic. The early contribution of eLa and",
                   "OS is the sliver at the onset.")),
  fitter = list(
    label = "Z′max raised to 75",
    design = list(vo2max = 75),
    inputs = list(ptype = "continuous", intensity = 105, duration = 180,
                  warmup = 10, recovery = 300),
    expect = paste("A wider tube Z. The same relative intensity is met with more",
                   "aerobic flow and less lactic, and the lactate accumulated",
                   "falls. Intensity is a percentage, so this athlete is working",
                   "at a higher absolute power.")),
  weak_gly = list(
    label = "A′max halved",
    inputs = list(ptype = "continuous", intensity = 150, duration = 60,
                  lactic_power = 22, warmup = 10, recovery = 300),
    expect = paste("Tube A is narrower. U′ is unchanged, so P must fall further",
                   "and the alactic contribution grows. Reservoirs trade.")),
  floating = list(
    label = "Floating tube A",
    inputs = list(ptype = "continuous", intensity = 150, duration = 60,
                  float_tubes = TRUE, warmup = 10, recovery = 300),
    expect = paste("A rides on P's surface rather than sitting at a fixed height,",
                   "so A′ starts at zero and grows only as P falls. The",
                   "activation delay comes from the geometry, not a time",
                   "constant.")),
  jeremy = list(
    label = "Jeremy's defaults, against the 400 m",
    preset = "jeremy",
    expect = paste("Loads the second preset and the men's 400 m case figure.",
                   "These parameters were adjusted to follow the case figures",
                   "more closely than the chapter's reference athlete does:",
                   "L₁ raised from 0.335 to 0.535, a larger P tapering toward",
                   "its top, a taller Gl body under a short eLa wedge, a wider",
                   "O₂ store, floating tubes, an Ox tap that takes 20 s and a",
                   "store capped at 25% of V̇O₂max. Compare the simulated",
                   "partition against the dashed reference curves, then switch",
                   "the defaults back to di Prampero and run it again.")),
  intervals = list(
    label = "12 × 15 s, 15 s recovery",
    inputs = list(ptype = "intermittent", intensity = 200, work = 15, rest = 15,
                  nreps = 12, rest_int = 0, warmup = 10, recovery = 300),
    expect = paste("The one-way valve means Gl does not refill between bouts.",
                   "Each repetition starts from a lower level, A′ falls, and Z′",
                   "carries progressively more of the work."))
)


# ---- helpers ----------------------------------------------------------------

hy2_tour_note <- function(...) htmltools::tags$div(class = "hy-tour-note", ...)

#' A term and its meaning, side by side
hy2_gloss <- function(...) {
  rows <- list(...)
  htmltools::tags$div(
    class = "hy-gloss",
    lapply(rows, function(r) htmltools::tagList(
      htmltools::tags$div(class = "hy-gloss-k", htmltools::HTML(r[[1]])),
      htmltools::tags$div(class = "hy-gloss-v", htmltools::HTML(r[[2]])))))
}

# a plain arrow cursor, drawn rather than fetched
HY2_CURSOR <- htmltools::HTML(
  '<svg viewBox="0 0 20 24" width="19" height="23" aria-hidden="true">
     <path d="M2 1 L2 19 L7 14.5 L10.5 22 L14 20.3 L10.6 13.2 L17 12.7 Z"
           fill="#111821" stroke="#ffffff" stroke-width="1.6"
           stroke-linejoin="round"/></svg>')

#' An annotated screenshot of the running app
#'
#' @param src File under www/tour/.
#' @param width Rendered width, CSS.
#' @param steps List of `list(at = c(x, y), text = ...)`, `at` in per-cent of the
#'   picture. The positions come from `HY2_TOUR_MARKS`, generated alongside the
#'   screenshots from the same DOM, so they cannot drift from what they point at.
hy2_shot <- function(src, width, steps) {
  n <- seq_along(steps)
  htmltools::tags$div(
    class = "hy-shot",
    htmltools::tags$div(
      class = "hy-shot-frame", style = sprintf("max-width:%s;", width),
      htmltools::tags$img(src = src, class = "hy-shot-img", alt = ""),
      lapply(n, function(i) {
        at <- steps[[i]]$at
        if (is.null(at)) return(NULL)
        htmltools::tags$div(
          class = "hy-shot-mark",
          style = sprintf("left:%.2f%%; top:%.2f%%;", at[1], at[2]),
          HY2_CURSOR, htmltools::tags$span(class = "hy-shot-num", i))
      })),
    htmltools::tags$ol(
      class = "hy-shot-steps",
      lapply(n, function(i) htmltools::tags$li(steps[[i]]$text))))
}

#' One guided experiment
hy2_lab <- function(key) {
  L <- HY2_TOUR_LABS[[key]]
  htmltools::tags$div(
    class = "hy-lab-card",
    htmltools::tags$div(
      class = "hy-lab-head",
      htmltools::tags$span(class = "hy-lab-name", L$label),
      shiny::actionButton(paste0("tour_load_", key), "load",
                          class = "btn hy-pill hy-pill-accent btn-sm",
                          icon = shiny::icon("sliders"))),
    htmltools::tags$div(class = "hy-lab-expect", L$expect))
}

.mk <- function(grp, key) {
  if (!exists("HY2_TOUR_MARKS")) return(NULL)
  HY2_TOUR_MARKS[[grp]][[key]]
}


# ---- the steps --------------------------------------------------------------

HY2_TOUR <- list(

  list(
    key = "model", nav = "The model", icon = "diagram-3",
    title = "A hydraulic model of muscular energetics",
    body = function() htmltools::tagList(
      htmltools::tags$p(
        class = "hy-lede",
        "The energetics of muscular exercise can be represented ",
        "semi-quantitatively by a set of reservoirs joined by tubes, each ",
        "reservoir standing for one energy-yielding mechanism."),
      hy2_gloss(
        c("Reservoir", "The capacity of a mechanism: how much energy it holds."),
        c("Tube", paste("Its resistance, and so the flow it can carry. Flow is",
                        "proportional to the difference in level between the",
                        "reservoirs the tube joins.")),
        c("The tap <b>S</b>", paste("Under P. Its opening sets the outflow",
                                    "<b>U′</b>, the work intensity.")),
        c("<b>L₀, L₁, L₂</b>", paste("Reference levels: rest, the height of the",
                                     "aerobic tube, the height of tube A."))),
      htmltools::tags$p(
        "The arrangement is di Prampero's Fig. I-6.1. Hydraulic analogues of ",
        "muscular energetics have a long tradition: Margaria (1975) describes a ",
        "very similar model, later reworked by Morton (1986)."),
      hy2_tour_note(
        "The model gives a reasonably faithful ", htmltools::tags$b("qualitative"),
        " description of exercise energetics. It cannot be pushed to rigorous ",
        "quantitative analysis. The kinetics of the individual mechanisms come out ",
        "distorted. Its value is conceptual and didactic.")
    )),

  list(
    key = "poxz", nav = "P, Ox and the tube Z", icon = "1-circle",
    title = "Two reservoirs, and what happens when S opens",
    body = function() htmltools::tagList(
      hy2_gloss(
        c("<b>P</b>", paste("The anaerobic alactic sources: ATP and PCr taken",
                            "together, without distinguishing them. Its capacity",
                            "is limited, but its outflow resistance is by far the",
                            "lowest in the figure, so U′ can reach very high",
                            "values.")),
        c("<b>Ox</b>", paste("The aerobic sources, joined to P by tube <b>Z</b>",
                             "at level L₁. Its capacity is by far the largest, so",
                             "its own level is effectively fixed whatever flows",
                             "through Z. Z's resistance is much greater than P's",
                             "outflow resistance."))),
      htmltools::tags$p(
        "Take only these two. With S closed both stand at L₀. Open S: U′ rises ",
        "as a square wave, the level in P falls below L₀, and a flow ",
        htmltools::tags$b("Z′"), " appears through Z, proportional to the level ",
        "difference. Z′ rises exponentially towards a steady state where ",
        "Z′ = U′, provided U′ does not exceed the maximal flow Z′max. P then ",
        "holds a constant level between L₀ and L₁."),
      htmltools::tags$div(class = "hy-panel-title", "At rest"),
      shiny::plotOutput("tour_rest", height = "260px"),
      htmltools::tags$div(class = "hy-panel-title mt-3", "20 s into work"),
      shiny::plotOutput("tour_work", height = "260px"),
      htmltools::tags$p(
        "Opening S is the analogue of the nervous command that sets work ",
        "intensity within a fraction of a second. The immediate fall in ",
        "high-energy phosphates, essentially a fall in PCr, drives a ",
        "progressive rise in O₂ uptake, which gradually offsets that fall and ",
        "finally halts it, provided the intensity stays below a given limit."),
      hy2_tour_note(
        "At the end of work the film runs backwards. P refills, Z′ falls in ",
        "proportion, and ceases when P is back at L₀. The time integral of Z′ ",
        "over that phase is the ", htmltools::tags$b("O₂ debt;"), " the time integral of U′ − Z′ at the onset is the ",
        htmltools::tags$b("O₂ deficit."), " Both are read above resting values.")
    )),

  list(
    key = "gl", nav = "Above Z′max: Gl and A", icon = "2-circle",
    title = "When U′ exceeds the aerobic maximum",
    body = function() htmltools::tagList(
      htmltools::tags$p(
        class = "hy-lede",
        "As soon as the level in P falls below L₁, the flow through Z reaches ",
        "Z′max and cannot rise further, whatever P does. That is the hydraulic ",
        "analogue of maximal O₂ uptake."),
      htmltools::tags$p(
        htmltools::tags$b("Gl,"), " the glycolytic sources, is joined to P by ",
        "tube ", htmltools::tags$b("A"), " at level L₂, below L₁. Once P falls ",
        "below L₁ a flow A′ appears from Gl to P. Provided U′ stays below ",
        "Z′max + A′max, P stabilises between L₁ and L₂."),
      htmltools::tags$p(
        "That state cannot last. Gl's capacity is small next to Ox's, and part ",
        "of it cannot be used at all, because the lower portion of Gl lies below ",
        "tube A. This is the counterpart of intramuscular glycogen being ",
        "converted anaerobically to lactic acid: the resulting acidification, ",
        "past a certain point, blocks its complete anaerobic use. Lactic ",
        "exercise therefore lasts about a third of what the muscle's 15 to 20 g/kg ",
        "of glycogen would otherwise allow."),
      htmltools::tags$p(
        "Open S further, until U′ exceeds Z′max + A′max, and P cannot stabilise ",
        "at all: it goes on falling past L₂ until the outflow ceases or the ",
        "intensity is reduced. Maximal lactic power has been reached. Beyond it, ",
        "further intensity adds nothing to the rate of blood lactate ",
        "accumulation."),
      hy2_tour_note(
        htmltools::tags$b("Recovery from supramaximal work. "),
        "While P is below L₂ both inflows are maximal. Above L₂ but below L₁, ",
        "A′ < A′max while Z′ is still maximal. Above L₁, A′ ceases and ",
        "Z′ < Z′max. Accordingly O₂ uptake stays maximal for 10 to 35 s after ",
        "supramaximal exercise, depending on its intensity."),
      htmltools::tags$p(
        "A ", htmltools::tags$b("one-way valve"), " in tube A stops Gl refilling ",
        "as P rises. Repaying the lactic debt means removing H⁺ and ",
        "resynthesising muscle glycogen, by routes not drawn in the figure and ",
        "with quite different kinetics from the alactic debt."),

      htmltools::tags$div(class = "hy-panel-title mt-4",
                          "Where tube A sits"),
      htmltools::tags$p(
        "The chapter fixes A at L₂. Its flow follows the head between the column ",
        "and P, and saturates at A′max once that head passes a reference value."),
      htmltools::tags$p(
        "The ", htmltools::tags$b("floating tube"), " option puts A on P's own ",
        "surface. The head is then the depth of lactic fluid standing above P, ",
        "divided by the column height:"),
      htmltools::tags$div(class = "text-center my-2",
                          htmltools::tags$b("A′ = A′max · (h_La − h_P)⁺ / column height")),
      htmltools::tags$div(class = "hy-panel-title", "Fixed at L₂"),
      shiny::plotOutput("tour_tube_fixed", height = "250px"),
      htmltools::tags$div(class = "hy-panel-title mt-3", "Floating on P"),
      shiny::plotOutput("tour_tube_float", height = "250px"),
      htmltools::tags$p(class = "hy-note",
        "The same run at 20 s. Tube A is the horizontal line running from the ",
        "column into P: at the foot of the vessel when fixed, at P's surface ",
        "when floating."),
      htmltools::tags$p("Three consequences:"),
      htmltools::tags$ul(class = "hy-list",
        htmltools::tags$li("A′ starts at zero, since at rest nothing stands ",
                           "above P. The activation delay follows from the ",
                           "geometry, so τ for glycolysis can stay at 0."),
        htmltools::tags$li("A′ grows as P falls, instead of saturating.")),
      shiny::plotOutput("tour_tube_flow", height = "270px"),
      hy2_tour_note(
        "At the default geometry, with L₂ at the floor and the column spanning ",
        "the vessel, a fixed tube whose reference head is 1 gives the same A′ as ",
        "the floating one. They differ once the column no longer spans the vessel.")
    )),

  list(
    key = "osela", nav = "OS and eLa", icon = "3-circle",
    title = "Two refinements",
    body = function() htmltools::tagList(
      htmltools::tags$p(
        htmltools::tags$b("OS,"), " the body's O₂ stores, essentially O₂ bound to ",
        "the haemoglobin of venous blood, is joined to P by tube ",
        htmltools::tags$b("Z₁,"), " at the same level as Z and with identical ",
        "resistance. At rest it stands at L₀. When S opens, its contribution is ",
        "set by the fall in P and by the shape and volume of OS, which follow ",
        "the haemoglobin dissociation curve and the venous blood volume. Z′ is ",
        "slowed in direct proportion."),
      hy2_tour_note(
        "Mixed venous O₂ content falls from about 150 mL/L at rest to about ",
        "50 mL/L in aerobic exercise near maximum, so each litre of venous blood ",
        "gives up about 100 mL. Roughly 4 of a 5-litre blood volume is venous: ",
        "about 400 mL in all. That O₂ is already inside the body and is not seen ",
        "at the lung, which is why muscle O₂ uptake exceeds mouth O₂ uptake while ",
        "the deficit is being contracted. At steady state OS and P settle at the ",
        "same level, net flow through Z₁ ceases, and Z′ = U′."),
      htmltools::tags$p(
        htmltools::tags$b("eLa,"), " early lactate, sits on the upper part of ",
        "Gl. On opening S it contributes an inflow to P proportional to the ",
        "level difference, slowing Z′, and ceases once P, and so eLa, settles ",
        "between L₀ and L₁. This is the initial lactic contribution that raises ",
        "blood lactate early in aerobic exercise, even of moderate intensity. ",
        "Its one-way valve, like Gl's, prevents refilling through tube A; both ",
        "refill by other routes, not drawn."),
      shiny::plotOutput("tour_schematic", height = "330px")
    )),

  list(
    key = "reading", nav = "Reading the figure", icon = "rulers",
    title = "Area, level, and the energy scales",
    body = function() htmltools::tagList(
      hy2_gloss(
        c("Area", paste("The energy a reservoir holds. The boxes under the",
                        "figure convert each drawn area into J·kg⁻¹ and compare",
                        "it with a reference athlete.")),
        c("Height of the fluid", paste("How full it is. Level is what drives the",
                                       "tubes, so it is the level, not the",
                                       "amount, that sets the flows.")),
        c("Shape", paste("How the two relate. A reservoir narrow at the top loses",
                         "much level for little energy, so the tubes below it",
                         "open early.")),
        c("The scales", paste("P on the left, eLa and Gl on the right. Each reads",
                              "energy <b>spent from full</b>, 0 at that",
                              "reservoir's own top. The lactic column drains",
                              "downward, so eLa's scale is exhausted before Gl's",
                              "begins."))),
      htmltools::tags$div(class = "hy-panel-title", "Straight-sided P"),
      shiny::plotOutput("tour_shape_cyl", height = "260px"),
      htmltools::tags$div(class = "hy-panel-title mt-3", "P narrowed at the top"),
      shiny::plotOutput("tour_shape_cone", height = "260px"),
      htmltools::tags$p(class = "hy-note",
        "Same height, 244 rather than 376 J·kg⁻¹, and the ticks bunch toward the ",
        "bottom: the first 50 J·kg⁻¹ spent now drops the level almost half way.")
    )),

  list(
    key = "design", nav = "Changing the athlete", icon = "sliders",
    title = "Change the hydraulic configuration directly on the figure",
    body = function() htmltools::tagList(
      hy2_shot(
        "tour/config_card.png", "760px",
        list(
          list(at = .mk("config", "nudge"), text = htmltools::HTML(
            "Every dimension has a <b>−/+</b> pair beside it. Press them and the ",
            "reservoir is redrawn; because area is energy, its capacity follows.")),
          list(at = .mk("config", "refs"), text = htmltools::HTML(
            "The boxes track what you have drawn, in J·kg⁻¹, against a reference ",
            "athlete.")),
          list(at = .mk("config", "apply"), text = htmltools::HTML(
            "<b>Apply configuration</b>, then <b>Run simulation</b>. Until you ",
            "apply you are only editing the drawing. An amber <b>not applied</b> ",
            "badge in the header says so.")))),
      htmltools::tags$p(class = "hy-note",
        "Most useful to begin with: V̇O₂max (the flow tube Z can carry), P width ",
        "at base (the alactic capacity), and the two Gl widths. ",
        htmltools::tags$b("Reset figure"), " restores the reference athlete.")
    )),

  list(
    key = "run", nav = "Running a simulation", icon = "play-circle",
    title = "The panel, top to bottom",
    body = function() htmltools::tagList(
      hy2_shot(
        "tour/panel_exercise.png", "300px",
        list(
          list(at = .mk("panel", "ptype"), text = htmltools::HTML(
            "<b>Continuous</b> or <b>intermittent</b> square waves; <b>case ",
            "figure</b> replays a measured demand curve, either a laboratory ",
            "effort from Gastin (2001) or one of the six Berlin-2009 sprint ",
            "finals from Briand et al. (2025). It sets that study's V̇O₂max and ",
            "draws the partition published with it over yours.")),
          list(at = .mk("panel", "intensity"), text = htmltools::HTML(
            "Intensity as a percentage of V̇O₂max. Below 100%, Z′ can eventually ",
            "cover U′ alone; above it, the balance must come from Gl and P.")),
          list(at = .mk("panel", "duration"), text = htmltools::HTML(
            "Duration, and the quiet time either side. Keep the recovery ",
            "generous, since the debt is paid there.")),
          list(at = .mk("panel", "info"), text = htmltools::HTML(
            "Each heading carries an <b>ⓘ</b> with the detail for that control.")),
          list(at = .mk("panel", "run"), text = htmltools::HTML(
            "<b>Run simulation</b>. Nothing recalculates until you press it; an ",
            "amber marker appears beneath when a setting has moved since the run ",
            "on screen."))))
    )),

  list(
    key = "figures", nav = "The three figures", icon = "graph-up",
    title = "What each figure answers",
    body = function() htmltools::tagList(
      htmltools::tags$div(class = "hy-panel-title",
                          "Energy supply: what met the demand"),
      shiny::plotOutput("tour_supply", height = "300px"),
      htmltools::tags$p(class = "hy-note",
        "Black is U′. The bands are stacked, so they sum to it. If P empties and ",
        "the inflows still cannot cover U′, a grey band appears on top: that is ",
        "demand nothing supplied, and the effort is not being sustained."),
      hy2_shot(
        "tour/transport.png", "100%",
        list(
          list(at = .mk("transport", "play"), text = htmltools::HTML(
            "<b>Play</b> sweeps the clock from the start, and the hydraulic ",
            "figure at the top of the page drains in step. Until you press it, a ",
            "finished run leaves the reservoirs full.")),
          list(at = .mk("transport", "speed"),
               text = "Simulated seconds per real second."),
          list(at = .mk("transport", "scrub"),
               text = "Or drag to park at an instant."))),
      htmltools::tags$div(class = "hy-panel-title mt-4",
                          "Energy expended: the time integrals"),
      shiny::plotOutput("tour_cum", height = "280px"),
      htmltools::tags$p(class = "hy-note",
        "The end of a curve is that pathway's total. The gap between the demand ",
        "and the aerobic curve is the O₂ deficit."),
      htmltools::tags$div(class = "hy-panel-title mt-4",
                          "Pathway power: each flow from zero"),
      shiny::plotOutput("tour_lines", height = "280px"),
      htmltools::tags$p(class = "hy-note",
        "Unstacked, for peaks, decay rates and crossovers, which a moving ",
        "baseline hides.")
    )),

  list(
    key = "numbers", nav = "The four numbers", icon = "123",
    title = "Totals for the run",
    body = function() htmltools::tagList(
      htmltools::tags$img(src = "tour/metrics.png", class = "hy-shot-plain",
                          alt = ""),
      hy2_gloss(
        c("<b>peak aerobic power</b>", paste(
          "The highest Z′ + Z₁′ reached, in mL O₂·kg⁻¹·min⁻¹ above rest, and as",
          "a percentage of Z′max. 100% means the aerobic maximum was actually",
          "reached, which short efforts do not manage.")),
        c("<b>lactic energy</b>", paste(
          "The time integral of A′, its share of the total, and the blood",
          "lactate it implies (E / 3 / 20.9).")),
        c("<b>alactic energy</b>", paste(
          "Net splitting of high-energy phosphates, and how far P was drawn",
          "down. It can never exceed P's capacity: once P is empty the level",
          "cannot fall further, so nothing more can come from it.")),
        c("<b>aerobic / lactic / alactic</b>", paste(
          "The three shares of what was <b>supplied</b>. Demand that nothing",
          "met is counted in none of them and is reported beside the total as",
          "<b>unmet</b>."))),
      hy2_tour_note(
        "These totals run to the end of the simulation, ",
        htmltools::tags$b("recovery included,"), " and the aerobic system does ",
        "much of its work after S closes. A 10 s all-out effort is 25/33/43 over ",
        "the work itself but 47/23/30 once 4 min of recovery is counted. ",
        "Published figures for the contribution of a sprint mean the first. Set ",
        "the recovery to 0 to compare with one.")
    )),

  list(
    key = "experiments", nav = "Seven experiments", icon = "beaker",
    title = "Things worth trying",
    body = function() htmltools::tagList(
      htmltools::tags$p(class = "hy-lede",
        "Each button fills the panel in and switches to the Simulator; press ",
        htmltools::tags$b("Run simulation"), " there. All but the last build on ",
        "whichever configuration ", htmltools::tags$b("defaults"), " is set to, ",
        "so they read against the chapter unless you change it."),
      htmltools::tags$div(class = "hy-lab-grid",
        hy2_lab("sprint"), hy2_lab("steady"), hy2_lab("fitter"),
        hy2_lab("weak_gly"), hy2_lab("floating"), hy2_lab("intervals"),
        hy2_lab("jeremy"))
    )),

  list(
    key = "glossary", nav = "Symbols and sources", icon = "book",
    title = "Symbols, units and sources",
    body = function() htmltools::tagList(
      htmltools::tags$div(class = "hy-panel-title", "Symbols"),
      hy2_gloss(
        c("<b>P</b>", "Anaerobic alactic sources: ATP + PCr."),
        c("<b>Ox</b>", "Aerobic sources. Capacity effectively infinite."),
        c("<b>OS</b>", "O₂ stores, largely O₂ bound to venous haemoglobin."),
        c("<b>Gl</b>", "Glycolytic sources."),
        c("<b>eLa</b>", "Early lactate; the upper part of the lactic column."),
        c("<b>S</b>, <b>U′</b>", "The tap, and the outflow it sets: work intensity."),
        c("<b>Z</b>, <b>Z′</b>, <b>Z′max</b>", paste(
          "The aerobic tube, its flow, and the maximal flow through it, the",
          "analogue of maximal O₂ uptake.")),
        c("<b>Z₁</b>, <b>Z₁′</b>", "The tube from the O₂ stores, and its flow."),
        c("<b>A</b>, <b>A′</b>, <b>A′max</b>",
          "The lactic tube, its flow, and the maximal lactic power."),
        c("<b>L₀, L₁, L₂</b>", "Rest; the height of Z and Z₁; the height of A."),
        c("<b>τ</b>", "A time constant: how long a tube's flow takes to respond.")),
      htmltools::tags$div(class = "hy-panel-title mt-4", "Units"),
      hy2_gloss(
        c("<b>W·kg⁻¹</b>", "A rate of energy expenditure per kilogram of body mass."),
        c("<b>J·kg⁻¹</b>", "An amount of energy per kilogram. Rate × time."),
        c("<b>mL·kg⁻¹·min⁻¹</b>", paste(
          "The physiologist's unit for aerobic rate. 1 mL O₂ releases about",
          "20.9 J, which is how the app converts to W·kg⁻¹.")),
        c("<b>mmol·L⁻¹</b>", "Blood lactate concentration. Rest ≈ 1.")),
      htmltools::tags$div(class = "hy-panel-title mt-4", "Sources"),
      htmltools::tags$ul(class = "hy-list",
        htmltools::tags$li("di Prampero, P. E. (2015), ",
          htmltools::tags$i("La locomozione umana su terra, in acqua, in aria. ",
                            "Fatti e teorie,"), " 2nd ed., Milano: Edi·Ermes, ",
          "224 pp., ISBN 978-88-7051-476-6. Chapter I-6, ",
          htmltools::tags$i("L'energetica del lavoro muscolare: un modello ",
                            "idraulico,"), " Fig. I-6.1, is the model and this ",
          "tour's account of it. ",
          htmltools::tags$a(
            href = "https://www.ediermes.it/it/catalogo-libri/fisiologia/la-locomozione-umana-su-terra,-in-acqua,-in-aria-detail",
            target = "_blank", rel = "noopener", "Publisher's page"), "."),
        htmltools::tags$li("Margaria, R. (1975), ",
          htmltools::tags$i("Fisiologia muscolare e meccanica del movimento,"), " pp. 65-67. An earlier, very similar model, reworked by Morton (1986)."),
        htmltools::tags$li("Briand, di Prampero, Osgnach, Thibault, Tremblay ",
          "et al. (2025), ", htmltools::tags$i("Eur J Appl Physiol"),
          " 125:3521-3541. The reference reservoir capacities and the energy ",
          "equivalents, and the sprint case figures, in which the demand is ",
          "that paper's metabolic power and the dashed curves are its ",
          "three-pathway partition of it."),
        htmltools::tags$li("Gastin, P. (2001), ", htmltools::tags$i("Sports Med"),
          " 31:725-741, Figs. 4 and 5. The measured demand curves behind the ",
          "laboratory case figures.")),
      hy2_tour_note(
        "The model gives a reasonably faithful qualitative description of ",
        "exercise energetics but cannot be pushed to rigorous quantitative ",
        "analysis: the kinetics of the individual mechanisms come out distorted. ",
        "Read the outputs accordingly.")
    ))
)

hy2_tour_n <- function() length(HY2_TOUR)
