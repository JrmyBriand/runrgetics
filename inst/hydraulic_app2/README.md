# Hydraulic simulator — v2

A single page: design the reservoirs on the figure, drive them with an exercise,
read what each pathway supplied.

```r
MuscleEdot::run_hydraulic_app2()
# or, from the source tree
shiny::runApp("ModelR/package/inst/hydraulic_app2")
```

## Where this app is published

MuscleEdot is the source of truth for the app, and it is **vendored** into
[runrgetics](https://github.com/JrmyBriand/runrgetics), the public package,
which is what a hosted deployment or a colleague can install from. It is
self-contained: no `system.file`, no datasets, no `MuscleEdot::` calls, and no
name collisions with runrgetics. 516 KB, 18 files.

```sh
Rscript inst/hydraulic_app2/sync_to_runrgetics.R           # check for drift
Rscript inst/hydraulic_app2/sync_to_runrgetics.R --write   # copy over
```

The copy must dereference the symlinks: `inst/hydraulic_app2/R` points into
`inst/hydraulic_app/R` here, and neither an R package build nor Windows carries
a symlink across. `test-hydraulic_app2_vendored.R` fails if the copies drift,
and skips when runrgetics is not on the machine.

## The source

di Prampero, P. E. (2015). *La locomozione umana su terra, in acqua, in aria.
Fatti e teorie*, 2nd ed. Milano: Edi·Ermes. 224 pp. ISBN 978-88-7051-476-6.
[Publisher's page](https://www.ediermes.it/it/catalogo-libri/fisiologia/la-locomozione-umana-su-terra,-in-acqua,-in-aria-detail)

Chapter I-6, *L'energetica del lavoro muscolare: un modello idraulico*,
Fig. I-6.1, is the model this app implements.

## The model is shared, deliberately

`R/` holds **symbolic links** to `../hydraulic_app/R/`:

```
core.R  designer.R  gastin_cases.R  sprint_cases.R  schematic.R  schematic_design.R
```

so the two apps cannot simulate different physics. Only `v2_plots.R` and
`v2_theme.R` are new here. Edit the model in `inst/hydraulic_app/R/` and both
apps follow; `tests/testthat/test-hydraulic_app_core.R` still checks that core
against the package's `deSolve` implementation in `R/hydraulic_model.R`.

## Case figures

Two families sit behind the **case figure** exercise type, in one grouped
dropdown. Both hand the simulator a measured demand and draw the partition
published with it as dashed lines over the simulated stack — the point being to
work the panel until the two agree.

| family | demand | dashed curves | closes? |
|---|---|---|---|
| Gastin (2001), Figs. 4–5 | digitised from the figure | digitised from the same figure | exactly, by construction |
| Briand et al. (2025), Berlin 2009 | di Prampero metabolic power from the published splits | the paper's three-pathway fit **to that demand** | no — short by the fit residual |

The second row is the one to keep in mind. For a sprint case the demand is a
*measurement* and the three dashed curves are a *model of it*, so their sum
falls about a per cent below the demand line (1.6–6.0 W/kg RMSE). That daylight
is the published model's own residual, not a fault of the simulation, and it is
left visible on purpose.

The six cases are Bolt's 100 m and 200 m, Merritt's 400 m, Fraser-Pryce's 100 m,
Felix's 200 m and Williams' 400 m, with the maximal aerobic power published for
each sex (24.5 W/kg male, 21 W/kg female) written onto the design as V̇O₂max.

`gastin_cases.R` is a hand digitisation; `sprint_cases.R` is **generated** by
`data-raw/generate_sprint_cases.R` from the `runrgetics` package, and reproduces
it to 5e-5 W/kg on the supplies and 0.05 W/kg on the demand. Do not edit it by
hand — re-run the generator.

## What is different from v1

**Gone.** Every tab except the simulator. The case-figure comparison, the
floating-tube study and the equations page were working notes rather than a
tool, and none of them is needed to run a simulation.

Also gone from the panel: `tau_A` (glycolytic activation), `tau_Z1` (O₂-store
activation), `tau_mouth` / `delta_mouth` (the mouth transfer function),
`tau_clear`, `V_ratio` and `k_eff` (lactate distribution and clearance). All sat
at their defaults in every configuration that survived, so the panel was
inviting choices nobody was making. They keep their defaults in the model; the
Ox tap's `tau_Z` is the one time constant that stays adjustable.

`gA_head_ref` is now shown **only when tube A is fixed**, because with a
floating tube it is never read — see below.

**New.**

- **Gl's rim width.** Gl is drawn as a trapezoid: a width at the floor and a
  width at the rim. The model already supported this (`gl_taper`) but
  `design_to_params()` pinned it at 1, so the figure could not say it. The drawn
  area — which is the energy — follows.
- **The lactic column is right-aligned.** Gl and eLa share one vertical wall on
  the right — the one the energy scales sit against — and every width is taken
  off to its left, so all the shaping is on one side.
- **Energy scales on the reservoirs.** P's runs up its left wall; eLa's and
  Gl's run down the right. Each reads energy **spent from full**, 0 at that
  reservoir's own top, and the lactic column drains downward, so eLa's scale is
  exhausted before Gl's begins.
- **A ruler that follows the shape.** P's LEVEL is what drives Z′ and A′, but
  how much ENERGY a given drop costs was left to be guessed — and with a conic P
  the two are not proportional. The ticks are placed by integrating P's own
  width profile: a cylinder gives evenly spaced ticks (half the height IS half
  the energy), a cone bunches them toward the wide end. Opt-in
  (`hyd_schematic_design(ruler = TRUE)`), so v1's figure is untouched.
- **Cumulative pathway energy** in the form of di Prampero's Fig. 13.3: one
  curve per source against time, not stacked, with the demand as the envelope.
  Read the end of a curve for that pathway's total; read the gap between two
  curves for how far apart they are at that instant, which a stack cannot show
  because every layer above the first sits on a moving baseline.
- **Pathway power, unstacked** — the same three supplies from a common zero.
  Stacked bands answer "what made up the demand"; these answer "what was each
  pathway doing", which is the question behind a peak, a decay rate or a
  crossover. This replaced v1's V̇O₂-and-lactate figure.
- **Head-line numbers that are those integrals.** In v1 the value boxes
  integrated `Zp` while the figure drew `Zp + Z1p`; the numbers and the picture
  answered different questions. `hyd2_bands()` in `R/v2_plots.R` is now the one
  definition, used by both.
- **Blood-lactate accumulation** from the lactic energy, `E / (EQ_La · EQ_O2)`
  = `E / 3 / 20.9` mmol·L⁻¹.
- The head-line numbers sit **under** the figure they are the integrals of, and
  each carries its share of the total.
- **A finished run shows its whole self.** v1 parked the playback clock at 0, so
  both figures were blank until Play was pressed — which reads as a broken
  simulation rather than as frame one of an animation. The *schematic* is the
  exception: it stays at rest, with the reservoirs full, until Play or the
  scrubber is used (`animating`), because a drained figure sitting next to a
  design you are about to edit reads as the design rather than as the last frame
  of a run.
- **A stale-run marker** under the Run button when a setting has moved since the
  run on screen.
- Detail lives in **popovers** on the ⓘ markers, not in the panel.

### Two accounting rules the figures obey

**The alactic band is bounded by P.** `alactic_W` is `U′` minus the inflows,
which is the rate P's content falls, but only while P has something to give.
Once P is empty the ODE clamps `dQ_P` at zero and the shortfall is demand
nothing met. `hyd2_bands()` used to charge that shortfall to the alactic
pathway: a 100 m with `C_P` = 150 J·kg⁻¹ and `A′max` = 25 reported 331 J·kg⁻¹ of
alactic energy out of a 150 J·kg⁻¹ reservoir. The band is now capped by P's
remaining content (`Q_P / dt`, zero at the floor and inactive elsewhere, so the
step where P runs out is exact too) and the remainder is returned as `unmet`. It
is drawn as a grey band on top of the stack, added to the cumulative figure and
shown beside the total, but it is in none of the three shares, because it is not
a supply. `test-hydraulic_app2.R` asserts the alactic total never exceeds `C_P`
and always equals P's drawdown.

**Tube A sits at L₂, floating or not.** The fixed branches used `head`, already
clamped at L₂; the floating branch used `h_La − h_P` raw, so once P drained past
the column's own floor the head went on growing and the tube was drawn below the
column it draws from. Below L₂ there is no lactic fluid, only air, so the depth
standing above P stops growing there. No effect at the default L₂ = 0.

**Tube A is only broken where the O₂ store actually is.** A passes the OS tank
on its way from the lactic column to P, and is drawn broken there, the usual
convention for a pipe passing behind something. The test was the horizontal
overlap alone, which cut the *fixed* tube A in half too, although it sits down
at L₂, well below the tank's floor at L₁. It is now broken only when its level
falls inside the tank's own vertical extent.

**Z₁ bottoms out at L₁.** OS is joined to P "allo stesso livello di Z, e
caratterizzato da una identica resistenza idraulica" (chapter I-6). The store's
own tank spans L₁ to L₀, so `h_OS` can never fall below L₁; but the driving head
was `h_OS − h_P` with no clamp, so once P fell below L₁ the head went on growing
and the tube behaved as if it sat at the floor. It is now `h_OS − max(h_P, L₁)`.
At `tau_Z` = 0 nothing changes, because the `Z + Z₁ ≤ Z′max` ceiling had masked
it; at `tau_Z` = 10 to 25 s the store was delivering up to 11.4 W·kg⁻¹ on an
overstated head, worth about 0.15% of the aerobic total.

That head reads two equivalent ways, and they are the same arithmetic: a tube
welded at L₁ whose outlet is *submerged* while P stands above it, taking its head
down to P's surface and discharging freely once P drops below; or an intake that
rides on P's surface and bottoms out at L₁, the store's own floor, exactly as A
relates to the column's floor at L₂. The schematic draws the second, so the
picture shows the head the model is using rather than only its limiting case. So
A and Z₁ both ride on P, each stopping at its own floor, and only Z is drawn
welded at L₁. Z₁ is drawn that way in **both** tube configurations, because its
law never consults `float_tubes`; only A's conductance does. Above L₁ the two
floating tubes land on the same line, with OS between them, and read as one pipe
P → OS → column, so A is drawn a hair below to separate them (under 1% of the
level range).

**Z₁ maximal rate (% of V̇O₂max).** `z1_max_frac` caps the flow through Z₁ at a
fraction of Z′max. The chapter gives Z₁ the same resistance as Z and no ceiling
of its own, so the store is bounded only by the joint `Z + Z₁ ≤ Z′max`; 100%,
the default, is that case exactly. Below it the store is additionally capped, on
the reasoning that the cascade emptying the venous store (dissociation,
diffusion, convection) is not the cascade that fixes maximal O₂ uptake, so there
is no reason the two should share a ceiling.

It changes the store's *timing*, not the partition. At 200% V̇O₂max with
`tau_Z` = 20 s, 100% → 10% takes peak Z₁ from 8.8 to 2.1 W·kg⁻¹ and
half-emptying from 9.2 to 22 s, while lactic energy moves 1187 → 1189 J·kg⁻¹
(1190 with the store shut off entirely). Two things bound where it bites: it can
only cap below the flow the head and the joint ceiling already allow, about 40%
in that run, and at `tau_Z` = 0 the joint ceiling holds Z₁ near zero at high
intensity anyway, because Z is maximal and leaves nothing under it. For
calibration, half-peak a-vO₂ difference was 13 ± 2 s in exercising human thigh
(Bangsbo et al. 2000), against 17 s for this store at 100% and 100% V̇O₂max.

### The locked defaults

Two named configurations, both in code, in `R/v2_presets.R`, chosen with
**defaults** at the top of the panel.

**di Prampero** is `design_defaults()` unchanged: the chapter's reference
athlete at V̇O₂max 60 mL·kg⁻¹·min⁻¹, cylindrical P, rectangular Gl, tubes fixed,
no delay on the Ox tap. The app opens on it, and every statement in the tutorial
about "the chapter" refers to it.

**Jeremy's** is the set arrived at by fitting the case figures, locked on
19 August 2026: L₁ raised 0.335 → 0.535, a larger P (10 → 14) tapering toward
its top (14 → 8), a taller Gl body under a short eLa wedge (0.665 → 0.165), a
wider O₂ store, V̇O₂max 66.9, floating tubes, A′max 100 W·kg⁻¹, τ Ox 20 s and
Z₁ capped at 25%.

Choosing one loads its figure and its panel settings together; **Reset figure**
returns to whichever is selected. A preset states its design as *overrides* on
`design_defaults()`, so a design field added later is inherited at its reference
value rather than silently dropped, and an unknown or malformed name falls back
to the chapter rather than erroring.

This replaces `R/v2_prefs.R`, which stored one configuration in
`tools::R_user_dir("MuscleEdot", "config")`. That was the right tool for
deciding the defaults and the wrong one for shipping them: a hosted app has no
persistent per-user config directory, and a published figure should not depend
on a file that exists only on the machine it was tuned on. Nothing is written
outside the package any more, and a test asserts it.

## The tutorial tab

An eleven-step tour, in `R/v2_tour.R`, pitched at di Prampero's Chapter I-6 and
using its terminology: reservoirs and tubes, capacity and resistance, flow
driven by a level difference, `U′` as the outflow set by the tap `S`, `Z′max` as
the hydraulic analogue of maximal O₂ uptake, O₂ deficit and debt. Where the
chapter gives a number — venous O₂ content 150 → 50 mL/L, ~400 mL of stores, the
third of muscle glycogen reachable anaerobically, the 10–35 s of maximal O₂
uptake after supramaximal work — the tour gives the same one, and
`test-hydraulic_app2_tour.R` checks they are all still there. So is the
chapter's own caveat: the model is qualitative and didactic, and cannot be
pushed to rigorous quantitative analysis.

It is also a tour of an app, so it says where to click, using **real screenshots
of the running app** with a cursor at each point. `make_tour_shots.R` captures
them into `www/tour/` and, from the same DOM, writes the cursor positions to the
generated `R/v2_tour_marks.R` — so an annotation cannot drift from the thing it
points at. Re-run it after any change to the panel or the configuration card.

The steps are data (`HY2_TOUR`), not markup buried in `app.R`, so the sequence
can be reordered and the count is derived. Six guided experiments
(`HY2_TOUR_LABS`) each load a configuration into the panel and switch to the
Simulator, with a written prediction to read *before* running. Settings are
loaded, not run: updating an input is a client round-trip, so firing the
simulation from the server would run it against the previous values — and
pressing Run is the habit the tour is building anyway.

One thing the tour surfaced that is worth knowing independently of it: **the
head-line energy shares run to the end of the simulation, recovery included.**
A 10 s all-out effort is 25/33/43 over the bout but 47/23/30 once 240 s of
recovery is counted, because the aerobic system does substantial work after the
tap closes. Textbook values for "the contribution of a 10 s sprint" mean the
first kind. The split box's unit line now says which one it is, and step 8 of
the tour teaches the distinction with those numbers — which are asserted in
`test-hydraulic_app2_tour.R`, so the prose cannot drift from the model.

### Fixed vs floating tube A, in the tour

The step that introduces tube A now compares the two side by side: the same run
at 20 s drawn with A at L₂ and with A on P's surface, then A′ against time for
both. The figures carry the argument, so the prose is three sentences and three
bullets rather than three paragraphs.

The tour's prose uses no em or en dashes (page ranges in the citations keep a
plain hyphen); `test-hydraulic_app2_tour.R` asserts it.

## Is `gA_head_ref` useless?

Not quite, and the app now says exactly where it is and is not.

With a **floating** tube A the parameter is never read: the flow is
`A'max · (h_La − h_P)⁺ / column height`, a geometric normalisation with no free
gain. Runs at `gA_head_ref` 0.02 and 1.0 are bit-identical
(`test-hydraulic_app2.R`). The slider is therefore hidden in that mode.

With a **fixed** tube it is the tube's bore and a real lever. Over 20 s at 200%
V̇O₂max, moving it from 0.02 to 1.0 takes the lactic supply from 251 to
184 J·kg⁻¹ and peak A′ from 18.8 to 9.6 W·kg⁻¹. Those numbers are quoted in the
popover and asserted in the tests, so the two cannot drift apart.

A footnote worth knowing: at the default geometry (L₂ = 0, the column spanning
the full vessel) the fixed tube at `gA_head_ref = 1` reproduces the floating
tube exactly. The two readings only diverge once the column no longer spans the
vessel.

## Tests

`tests/testthat/test-hydraulic_app2.R` pins:

- that `app.R` passes nothing `design_to_params()` already sets — a duplicated
  formal argument kills the app at load with a blank page, and it has happened
  four times in this project (`os_ml_per_kg`, `gl_taper`, `ela_taper`,
  `p_taper`). The test reads the argument names out of `app.R` itself;
  reintroducing the bug makes it fail and name the culprit;
- that the head-line energies equal the integrals of the drawn bands, and that
  down-sampling for playback cannot move them;
- the two claims the panel makes about `gA_head_ref`;
- that a case figure sets V̇O₂max on the design, and puts it back;
- that clearing a numeric box does not take the app down.

`tests/testthat/test-hydraulic_app2_tour.R` pins the tour the same way: every
figure and button a step renders exists in the server, every experiment sets
inputs and design fields that exist, the glossary covers every symbol and unit
used, and the numbers the prose quotes at the reader are recomputed from the
model. It also pins the step machinery — clamping at both ends, and the
non-zero guard that stops a `renderUI` counter reset from jumping the step by
itself.
