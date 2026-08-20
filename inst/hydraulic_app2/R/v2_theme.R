# =============================================================================
# LOOK AND FEEL (v2)
#
# A dark control rail on the left, a light working surface on the right, cards
# with a quiet border rather than a shadow, and one accent colour used only for
# things you can act on. The pathway colours are NOT part of the chrome: aerobic
# blue / lactic orange / alactic green are carried over unchanged, because they
# mean something and are used in every other figure in this project.
# =============================================================================

HY2_RAIL   <- "#111821"
HY2_ACCENT <- "#12d3a0"

# ---- small UI helpers -------------------------------------------------------

#' Section heading inside the dark rail
hy2_section <- function(label, ...) {
  htmltools::tags$div(class = "hy-sec", htmltools::tags$span(label), ...)
}

#' A clickable info marker carrying the detail that used to clutter the panel
hy2_info <- function(title, ..., placement = "right") {
  bslib::popover(
    htmltools::tags$span(class = "hy-info", bsicons::bs_icon("info-circle"),
                         role = "button", tabindex = "0"),
    ..., title = title, placement = placement, options = list(customClass = "hy-pop")
  )
}

#' Label + info marker on one line
hy2_label <- function(label, info = NULL) {
  htmltools::tags$div(
    class = "hy-lab",
    htmltools::tags$span(label),
    if (!is.null(info)) info)
}

#' One column of the head-line metric strip
hy2_metric <- function(label, unit, value_id, sub_id, accent) {
  htmltools::tags$div(
    class = "hy-metric",
    htmltools::tags$div(
      class = "hy-metric-head",
      htmltools::tags$span(class = "hy-swatch",
                           style = sprintf("background:%s;", accent)),
      htmltools::tags$span(label)),
    htmltools::tags$div(class = "hy-metric-unit", unit),
    htmltools::tags$div(class = "hy-metric-value", shiny::textOutput(value_id, inline = TRUE)),
    htmltools::tags$div(class = "hy-metric-sub", shiny::uiOutput(sub_id, inline = TRUE))
  )
}

#' Card header: centred title with a leading icon, actions pushed right
hy2_card_head <- function(icon, title, ...) {
  htmltools::tags$div(
    class = "hy-head",
    htmltools::tags$div(class = "hy-head-spacer"),
    htmltools::tags$div(class = "hy-head-title",
                        bsicons::bs_icon(icon), htmltools::tags$span(title)),
    htmltools::tags$div(class = "hy-head-spacer hy-head-actions", ...)
  )
}


# ---- stylesheet -------------------------------------------------------------

hy2_css <- function() htmltools::tags$style(htmltools::HTML('
:root{
  --hy-bg:#f2f4f7; --hy-card:#ffffff; --hy-line:#e4e8ee; --hy-line-2:#eef1f5;
  --hy-ink:#1f2933; --hy-mute:#7b8794; --hy-faint:#9aa5b1;
  --hy-rail:#111821; --hy-rail-2:#1a2431; --hy-rail-line:#26313f;
  --hy-rail-ink:#e6edf5; --hy-rail-mute:#8b9aab;
  --hy-accent:#12d3a0; --hy-accent-ink:#06281f;
}
body, .btn, .form-control, .form-select, .popover, .irs{
  font-family:system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue",
              Arial, sans-serif;
}
body{ background:var(--hy-bg); color:var(--hy-ink); }

/* ============================ the dark rail ============================== */
.hy-rail{ padding:0 !important; }
.hy-rail .sidebar-content{ padding:0 !important; }
.hy-brand{
  display:flex; align-items:center; gap:.6rem;
  padding:1.05rem 1.1rem .95rem; border-bottom:1px solid var(--hy-rail-line);
}
.hy-brand-mark{
  width:30px; height:30px; border-radius:9px; flex:0 0 auto;
  background:linear-gradient(150deg, var(--hy-accent), #0aa5cf);
  display:flex; align-items:center; justify-content:center;
  color:var(--hy-accent-ink); font-size:1rem;
}
.hy-brand-name{ font-size:1.02rem; font-weight:650; letter-spacing:-.01em; line-height:1.1; }
.hy-brand-sub{ font-size:.68rem; color:var(--hy-rail-mute); line-height:1.2; }
.hy-rail-body{ padding:.25rem 1.1rem 1.1rem; }

.hy-sec{
  display:flex; align-items:center; gap:.4rem;
  margin:1.15rem 0 .55rem; padding-bottom:.35rem;
  border-bottom:1px solid var(--hy-rail-line);
  font-size:.72rem; font-weight:600; color:var(--hy-rail-mute);
  letter-spacing:.02em;
}
.hy-sec:first-child{ margin-top:.9rem; }

.hy-lab{
  display:flex; align-items:center; gap:.35rem;
  font-size:.79rem; color:var(--hy-rail-ink); margin-bottom:.3rem; font-weight:500;
}
.hy-info{ color:var(--hy-rail-mute); cursor:pointer; display:inline-flex; font-size:.82rem; }
.hy-info:hover{ color:var(--hy-accent); }
.hy-rail .form-label, .hy-rail label{ font-size:.79rem; font-weight:500; color:var(--hy-rail-ink); margin-bottom:.28rem; }
.hy-rail .shiny-input-container{ margin-bottom:.7rem; width:100% !important; }
.hy-rail .form-control, .hy-rail .form-select{
  background:var(--hy-rail-2); border:1px solid var(--hy-rail-line);
  color:var(--hy-rail-ink); border-radius:8px; font-size:.84rem; padding:.34rem .55rem;
}
.hy-rail .form-control:focus, .hy-rail .form-select:focus{
  border-color:var(--hy-accent); box-shadow:0 0 0 .15rem rgba(18,211,160,.18);
  background:var(--hy-rail-2); color:var(--hy-rail-ink);
}
.hy-rail .form-select{ background-image:url("data:image/svg+xml,%3csvg xmlns=\'http://www.w3.org/2000/svg\' viewBox=\'0 0 16 16\'%3e%3cpath fill=\'none\' stroke=\'%238b9aab\' stroke-linecap=\'round\' stroke-linejoin=\'round\' stroke-width=\'2\' d=\'m2 5 6 6 6-6\'/%3e%3c/svg%3e"); }

/* Segmented control instead of a row of dots. NOTE shiny still emits Bootstrap 3
   radio markup -- <div class="radio"><label><input><span> -- even under a BS5
   theme, so these selectors target that, not .form-check. */
.hy-seg .shiny-options-group{ display:flex; background:var(--hy-rail-2);
  border:1px solid var(--hy-rail-line); border-radius:9px; padding:2px; gap:2px; }
.hy-seg .radio{ flex:1 1 0; margin:0; padding:0; min-width:0; }
.hy-seg .radio label{ display:block; margin:0; width:100%; cursor:pointer; padding:0; }
.hy-seg .radio input[type="radio"]{ position:absolute; opacity:0; width:0; height:0;
  pointer-events:none; }
.hy-seg .radio span{ display:block; text-align:center; padding:.34rem .2rem;
  border-radius:7px; font-size:.76rem; color:var(--hy-rail-mute); white-space:nowrap;
  overflow:hidden; text-overflow:ellipsis; margin:0; transition:background .12s ease; }
.hy-seg .radio span:hover{ color:var(--hy-rail-ink); background:rgba(255,255,255,.05); }
.hy-seg .radio input:checked + span{ background:var(--hy-accent);
  color:var(--hy-accent-ink); font-weight:600; }
.hy-seg .shiny-input-container{ margin-bottom:.85rem !important; }

.hy-rail input[type="checkbox"], .hy-rail input[type="radio"]{ accent-color:var(--hy-accent); }
.hy-rail .checkbox label, .hy-rail .radio label{ font-size:.79rem; color:var(--hy-rail-ink);
  font-weight:500; display:flex; align-items:center; gap:.45rem; }
.hy-rail .checkbox input[type="checkbox"]{ width:15px; height:15px; margin:0; flex:0 0 auto; }

/* sliders */
.hy-rail .irs--shiny .irs-line{ background:var(--hy-rail-2); border:0; height:6px; top:27px; }
.hy-rail .irs--shiny .irs-bar{ background:var(--hy-accent); border:0; height:6px; top:27px; }
.hy-rail .irs--shiny .irs-handle{ background:#fff; border:2px solid var(--hy-accent);
  box-shadow:none; width:16px; height:16px; top:22px; }
/* the end labels collide with the value bubble whenever the handle sits near an
   end, which reads as a rendering fault; the bubble alone is enough */
.hy-rail .irs--shiny .irs-min, .hy-rail .irs--shiny .irs-max{ display:none; }
.hy-rail .irs--shiny .irs-single{ background:var(--hy-accent); color:var(--hy-accent-ink);
  font-size:.7rem; font-weight:600; border-radius:5px; }
.hy-rail .irs--shiny .irs-single:before{ border-top-color:var(--hy-accent); }
.hy-rail .irs{ font-family:inherit; }
.hy-rail .irs-grid{ display:none; }
.hy-rail .irs--shiny{ height:44px; }

.hy-rail hr{ border-color:var(--hy-rail-line); opacity:1; margin:.9rem 0; }
.hy-readout{ background:var(--hy-rail-2); border-radius:8px; padding:.45rem .6rem;
  margin:-.35rem 0 .75rem; }
.hy-readout .r{ display:flex; justify-content:space-between; font-size:.74rem;
  color:var(--hy-rail-mute); line-height:1.55; }
.hy-readout .r b{ color:var(--hy-rail-ink); font-weight:600; font-variant-numeric:tabular-nums; }

.hy-run{ display:grid; gap:.45rem; margin-top:1.15rem; }
.hy-run .btn-primary{ background:var(--hy-accent); border-color:var(--hy-accent);
  color:var(--hy-accent-ink); font-weight:650; border-radius:9px; padding:.5rem; }
.hy-run .btn-primary:hover{ filter:brightness(1.06); }
.hy-run .btn-ghost{ background:transparent; border:1px solid var(--hy-rail-line);
  color:var(--hy-rail-mute); border-radius:9px; font-size:.78rem; padding:.32rem; }
.hy-run .btn-ghost:hover{ color:var(--hy-rail-ink); border-color:#3a4a5d; }

/* ============================ working surface ============================ */
.bslib-sidebar-layout > .main{ padding:1.05rem 1.15rem 2rem; }
.card{ border:1px solid var(--hy-line); border-radius:14px;
  box-shadow:0 1px 2px rgba(16,24,40,.04); background:var(--hy-card); }
.card + .card{ margin-top:.85rem; }
.card-body{ padding:.85rem 1rem 1rem; }

.hy-head{ display:flex; align-items:center; padding:.85rem 1rem;
  border-bottom:1px solid var(--hy-line-2); background:transparent; }
.hy-head-spacer{ flex:1 1 0; display:flex; align-items:center; gap:.4rem; min-width:0; }
.hy-head-actions{ justify-content:flex-end; }
.hy-head-title{ display:flex; align-items:center; gap:.55rem; font-size:1.3rem;
  font-weight:680; color:var(--hy-ink); white-space:nowrap; letter-spacing:-.015em; }
.hy-head-title svg{ color:var(--hy-faint); }

/* head-line metric strip, laid out like a summary table */
.hy-metrics-card{ background:var(--hy-card); border:1px solid var(--hy-line);
  border-radius:14px; box-shadow:0 1px 2px rgba(16,24,40,.04); overflow:hidden;
  margin-bottom:.85rem; }
.hy-metrics-head{ display:flex; align-items:baseline; gap:.75rem; flex-wrap:wrap;
  padding:.6rem 1.15rem; border-bottom:1px solid var(--hy-line-2); background:#fbfcfd; }
.hy-run-name{ font-size:.95rem; font-weight:650; color:var(--hy-ink); }
.hy-run-meta{ font-size:.84rem; color:var(--hy-mute); margin-left:auto;
  font-variant-numeric:tabular-nums; }
.hy-metrics{ display:grid; grid-template-columns:repeat(4, minmax(0,1fr)); }
.hy-metric{ padding:.95rem 1.15rem 1.05rem; border-left:1px solid var(--hy-line-2);
  min-width:0; }
.hy-metric:first-child{ border-left:0; }
.hy-metric-head{ display:flex; align-items:center; gap:.45rem; font-size:.92rem;
  font-weight:550; color:var(--hy-ink); white-space:nowrap; overflow:hidden;
  text-overflow:ellipsis; }
.hy-swatch{ width:9px; height:9px; border-radius:2px; flex:0 0 auto; }
.hy-metric-unit{ font-size:.76rem; color:var(--hy-faint); margin-top:.1rem; }
.hy-metric-value{ font-size:2.35rem; font-weight:650; line-height:1.15; margin-top:.4rem;
  color:var(--hy-ink); font-variant-numeric:tabular-nums; letter-spacing:-.025em; }
.hy-metric-sub{ font-size:.88rem; color:var(--hy-mute); margin-top:.25rem;
  min-height:1.3rem; }
.hy-metric-sub b{ color:var(--hy-ink); font-weight:650; font-variant-numeric:tabular-nums; }

/* pill controls, as on the reference dashboard */
.hy-pill{ border-radius:999px; background:#fff; border:1px solid var(--hy-line);
  color:var(--hy-ink); font-size:.88rem; padding:.35rem .95rem; line-height:1.4; }
.hy-pill:hover{ background:#f6f8fa; border-color:#d3d9e2; color:var(--hy-ink); }
.hy-pill.active, .hy-pill:active{ background:var(--hy-accent); border-color:var(--hy-accent);
  color:var(--hy-accent-ink); font-weight:600; }
.hy-pill-accent{ background:var(--hy-accent); border-color:var(--hy-accent);
  color:var(--hy-accent-ink); font-weight:600; }
.hy-pill-accent:hover{ filter:brightness(1.05); background:var(--hy-accent);
  border-color:var(--hy-accent); color:var(--hy-accent-ink); }

/* the +/- controls laid over the schematic */
.hy-nudge{ position:absolute; transform:translate(-50%,-50%); pointer-events:auto;
  text-align:center; white-space:nowrap; background:rgba(255,255,255,.9);
  border:1px solid var(--hy-line); border-radius:8px; padding:2px 5px 3px;
  box-shadow:0 1px 2px rgba(16,24,40,.06); }
.hy-nudge-lab{ font-size:.63rem; line-height:1.15; font-weight:500; }
.hy-nudge .btn{ padding:0 6px; font-size:.72rem; line-height:1.35; border-radius:5px;
  border-color:var(--hy-line); color:var(--hy-mute); background:#fff; }
.hy-nudge .btn:hover{ background:var(--hy-accent); border-color:var(--hy-accent);
  color:var(--hy-accent-ink); }

/* reference read-out under the schematic */
.hy-refs{ display:grid; grid-template-columns:repeat(6, minmax(0,1fr)); gap:.6rem; }
.hy-ref{ border:1px solid var(--hy-line); border-radius:12px;
  padding:.75rem .6rem .8rem; text-align:center; background:#fff; min-width:0; }
.hy-ref-name{ font-size:1.02rem; font-weight:650; }
.hy-ref-val{ font-size:2.1rem; font-weight:680; line-height:1.15; margin-top:.2rem;
  font-variant-numeric:tabular-nums; color:var(--hy-ink); letter-spacing:-.025em; }
.hy-ref-unit{ font-size:.86rem; color:var(--hy-faint); }
.hy-ref-badge{ font-size:.9rem; margin-top:.25rem; font-weight:600; }
@media (max-width: 1100px){ .hy-refs{ grid-template-columns:repeat(3, minmax(0,1fr)); } }

.hy-transport{ display:flex; align-items:center; gap:.75rem; margin-top:.5rem; }
.hy-clock{ min-width:96px; font-variant-numeric:tabular-nums; font-weight:650;
  font-size:.95rem; color:var(--hy-ink); }
.hy-transport .form-group, .hy-transport .shiny-input-container{ margin-bottom:0 !important; }
.hy-transport .irs--shiny .irs-bar{ background:var(--hy-accent); border:0; }
.hy-transport .irs--shiny .irs-handle{ border:2px solid var(--hy-accent); background:#fff;
  box-shadow:none; }
.hy-transport .irs--shiny .irs-single{ background:var(--hy-accent); color:var(--hy-accent-ink); }
.hy-transport .irs--shiny .irs-single:before{ border-top-color:var(--hy-accent); }

.hy-note{ font-size:.92rem; color:var(--hy-mute); }
/* Bootstrap paints a popover through --bs-popover-bg, which this theme leaves
   unset: the panel came out transparent and its text landed on top of whatever
   was behind it. Set the token, not just the element background. */
.popover{ --bs-popover-bg:#ffffff; --bs-popover-header-bg:#ffffff;
  --bs-popover-border-color:var(--hy-line); background-color:#fff; }
.popover.hy-pop{ max-width:340px; border-radius:12px; z-index:2000;
  box-shadow:0 10px 28px rgba(16,24,40,.16); }
/* Bootstrap fades a popover in over 150 ms. That leaves it on a composited
   layer at partial opacity, and the page behind shows through its text. Popovers
   are reference material, not decoration -- paint them at once, flat. */
.popover.fade{ transition:none !important; opacity:1 !important; }
.popover.hy-pop .popover-header{ background:#fff; border-bottom:1px solid var(--hy-line-2);
  font-size:.82rem; font-weight:650; color:var(--hy-ink); border-radius:12px 12px 0 0; }
.popover.hy-pop .popover-body{ font-size:.79rem; color:#3e4c59; line-height:1.5;
  background:#fff; border-radius:0 0 12px 12px; }
.popover.hy-pop .popover-body p:last-child{ margin-bottom:0; }

/* "you have changed something since the last run" */
.hy-stale{ display:flex; align-items:center; justify-content:center; gap:.35rem;
  font-size:.71rem; color:#ffca7a; margin-top:-.15rem; }
.hy-stale-dot{ width:6px; height:6px; border-radius:50%; background:#ffca7a;
  flex:0 0 auto; }
.hy-badge-warn{ background:#fff4e5; color:#8a5300; border:1px solid #ffd9a8;
  border-radius:999px; padding:.15rem .6rem; font-size:.72rem; font-weight:600; }

@media (max-width: 900px){ .hy-metrics{ grid-template-columns:repeat(2, minmax(0,1fr)); } }

/* ================================ the tour =============================== */
/* the tab strip above the working surface */
.nav-underline{ border-bottom:1px solid var(--hy-line); margin-bottom:1rem;
  gap:.25rem; }
.nav-underline .nav-link{ color:var(--hy-mute); font-size:.95rem; font-weight:550;
  padding:.55rem .95rem; border:0; display:flex; align-items:center; gap:.4rem; }
.nav-underline .nav-link:hover{ color:var(--hy-ink); }
.nav-underline .nav-link.active{ color:var(--hy-ink); font-weight:650;
  border-bottom:2px solid var(--hy-accent); }

.hy-tour{ display:grid; grid-template-columns:236px minmax(0,1fr); gap:1.1rem;
  align-items:start; }
.hy-tour-nav{ position:sticky; top:1rem; background:var(--hy-card);
  border:1px solid var(--hy-line); border-radius:14px; padding:.7rem .55rem;
  box-shadow:0 1px 2px rgba(16,24,40,.04); }
.hy-tour-navhead{ font-size:.74rem; font-weight:650; color:var(--hy-faint);
  letter-spacing:.04em; text-transform:uppercase; padding:.15rem .55rem .5rem; }
.hy-tour-navlink{ display:flex; align-items:center; gap:.55rem; width:100%;
  padding:.4rem .55rem; border-radius:9px; color:var(--hy-mute);
  font-size:.87rem; text-decoration:none; line-height:1.3; }
.hy-tour-navlink:hover{ background:#f5f7f9; color:var(--hy-ink);
  text-decoration:none; }
.hy-tour-navnum{ flex:0 0 auto; width:20px; height:20px; border-radius:50%;
  background:#eef1f5; color:var(--hy-faint); font-size:.72rem; font-weight:650;
  display:flex; align-items:center; justify-content:center; }
.hy-tour-navlink.is-active{ background:rgba(18,211,160,.12); color:var(--hy-ink);
  font-weight:650; }
.hy-tour-navlink.is-active .hy-tour-navnum{ background:var(--hy-accent);
  color:var(--hy-accent-ink); }

.hy-tour-main{ background:var(--hy-card); border:1px solid var(--hy-line);
  border-radius:14px; box-shadow:0 1px 2px rgba(16,24,40,.04); overflow:hidden; }
.hy-tour-head{ padding:1.25rem 1.6rem .35rem; }
.hy-tour-step{ font-size:.78rem; font-weight:650; letter-spacing:.05em;
  text-transform:uppercase; color:var(--hy-accent); }
.hy-tour-title{ font-size:1.7rem; font-weight:680; letter-spacing:-.02em;
  color:var(--hy-ink); margin:.15rem 0 0; }
.hy-tour-body{ padding:.5rem 1.6rem 1.5rem; font-size:1rem; line-height:1.62;
  color:#3e4c59; max-width:none; }
.hy-tour-body p{ margin-bottom:.9rem; }
.hy-lede{ font-size:1.13rem; line-height:1.55; color:var(--hy-ink); }
.hy-tour-body b, .hy-tour-body strong{ color:var(--hy-ink); font-weight:650; }

.hy-tour-note{ background:#f4fbf8; border-left:3px solid var(--hy-accent);
  border-radius:0 10px 10px 0; padding:.75rem 1rem; margin:1.1rem 0;
  font-size:.97rem; line-height:1.55; color:#33443f; }
.hy-tour-note b{ color:#13413a; }

.hy-list{ margin:.2rem 0 1rem 1.1rem; padding:0; }
.hy-list li{ margin-bottom:.45rem; }
.hy-steps{ margin:.2rem 0 1rem 1.25rem; padding:0; }
.hy-steps > li{ margin-bottom:.7rem; padding-left:.2rem; }

.hy-panel-title{ font-size:.92rem; font-weight:650; color:var(--hy-ink);
  margin:.4rem 0 .35rem; }

/* term / meaning pairs */
.hy-gloss{ display:grid; grid-template-columns:minmax(120px, 190px) minmax(0,1fr);
  gap:.1rem .95rem; margin:.9rem 0 1.1rem;
  border-top:1px solid var(--hy-line-2); }
.hy-gloss-k{ padding:.55rem 0 .55rem; color:var(--hy-ink); font-size:.95rem;
  border-bottom:1px solid var(--hy-line-2); }
.hy-gloss-v{ padding:.55rem 0 .55rem; font-size:.95rem; line-height:1.5;
  border-bottom:1px solid var(--hy-line-2); }

.hy-table-wrap{ overflow-x:auto; margin:.9rem 0 1.1rem; }
.hy-table{ width:100%; border-collapse:collapse; font-size:.92rem; }
.hy-table th{ text-align:left; font-weight:650; color:var(--hy-ink);
  border-bottom:2px solid var(--hy-line); padding:.5rem .7rem .45rem;
  white-space:nowrap; }
.hy-table td{ border-bottom:1px solid var(--hy-line-2); padding:.55rem .7rem;
  vertical-align:top; line-height:1.45; }
.hy-dot-aer, .hy-dot-lac, .hy-dot-ala{ display:inline-block; width:9px; height:9px;
  border-radius:2px; margin-left:.35rem; vertical-align:middle; }
.hy-dot-aer{ background:#0072B2; }
.hy-dot-lac{ background:#D55E00; }
.hy-dot-ala{ background:#009E73; }

/* guided experiments */
.hy-lab-grid{ display:grid; grid-template-columns:repeat(2, minmax(0,1fr));
  gap:.8rem; margin:1rem 0; }
.hy-lab-card{ border:1px solid var(--hy-line); border-radius:12px;
  padding:.75rem .9rem .85rem; background:#fcfdfe; }
.hy-lab-head{ display:flex; align-items:center; justify-content:space-between;
  gap:.6rem; margin-bottom:.45rem; }
.hy-lab-name{ font-size:1rem; font-weight:650; color:var(--hy-ink); }
.hy-lab-expect{ font-size:.9rem; line-height:1.5; color:#4a5763; }
@media (max-width: 1250px){ .hy-lab-grid{ grid-template-columns:minmax(0,1fr); } }

.hy-tour-foot{ display:flex; align-items:center; justify-content:space-between;
  gap:1rem; padding:.85rem 1.6rem 1.1rem; border-top:1px solid var(--hy-line-2);
  background:#fbfcfd; }
.hy-tour-dots{ display:flex; gap:.32rem; flex-wrap:wrap; }
.hy-tour-dots .hy-dot{ width:7px; height:7px; border-radius:50%;
  background:#d7dde5; }
.hy-tour-dots .hy-dot.is-on{ background:var(--hy-accent); width:20px;
  border-radius:4px; }


/* annotated screenshots: the picture, a cursor at each point, a numbered list */
.hy-shot{ display:flex; flex-wrap:wrap; gap:1.1rem 1.4rem; align-items:flex-start;
  margin:1rem 0 1.1rem; }
.hy-shot-frame{ position:relative; flex:0 1 auto; min-width:0; }
.hy-shot-img{ display:block; width:100%; height:auto; border-radius:10px;
  border:1px solid var(--hy-line); box-shadow:0 1px 3px rgba(16,24,40,.07);
  background:#fff; }
.hy-shot-mark{ position:absolute; transform:translate(-2px,-1px);
  pointer-events:none; line-height:0; filter:drop-shadow(0 1px 2px rgba(0,0,0,.28)); }
.hy-shot-num{ position:absolute; left:13px; top:15px; width:19px; height:19px;
  border-radius:50%; background:var(--hy-accent); color:var(--hy-accent-ink);
  border:1.5px solid #fff; font-size:.68rem; font-weight:700; line-height:16px;
  text-align:center; display:block; }
.hy-shot-steps{ flex:1 1 260px; min-width:240px; margin:0; padding:0;
  list-style:none; counter-reset:hyshot; }
.hy-shot-steps li{ counter-increment:hyshot; position:relative;
  padding:0 0 .7rem 1.9rem; font-size:.95rem; line-height:1.5; }
.hy-shot-steps li:last-child{ padding-bottom:0; }
.hy-shot-steps li::before{ content:counter(hyshot); position:absolute; left:0;
  top:.12rem; width:19px; height:19px; border-radius:50%;
  background:var(--hy-accent); color:var(--hy-accent-ink); font-size:.68rem;
  font-weight:700; line-height:19px; text-align:center; }
.hy-shot-plain{ display:block; width:100%; height:auto; border-radius:10px;
  border:1px solid var(--hy-line); margin:.4rem 0 1rem; }

@media (max-width: 1000px){
  .hy-tour{ grid-template-columns:minmax(0,1fr); }
  .hy-tour-nav{ position:static; }
}

'))
