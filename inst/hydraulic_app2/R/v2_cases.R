# =============================================================================
# CASE FIGURES -- the two families, behind one interface
#
# gastin_cases.R and sprint_cases.R each publish a demand curve and a partition
# of it, in the same columns and the same units. Everything the app needs to
# know about either is here, so app.R does not branch on the family at every
# use and a new family means editing one file.
#
# Sourced after both (shiny loads R/ alphabetically, and g < s < v).
# =============================================================================

# Gastin's laboratory figures and the Berlin-2009 sprint finals answer the same
# three questions -- what demand, what partition of it, what aerobic ceiling --
# so the app asks them through one set of helpers instead of branching at every
# use. The only real difference is that a Gastin demand IS the sum of its three
# supplies (it was digitised that way) while a sprint demand is an independent
# measurement the supplies are a fit TO; see sprint_cases.R.
CASE_CHOICES <- list(
  "Gastin (2001) · laboratory"        = as.list(GASTIN_CASE_NAMES),
  "Briand et al. (2025) · Berlin 2009" = as.list(SPRINT_CASE_NAMES))

case_is_sprint <- function(key) !is.null(key) && !is.null(SPRINT_CASES[[key]])

# 0.05 s for a sprint: the alactic term peaks at exp(-0.4) = 0.67 s, which a
# quarter-second grid steps straight over.
case_data <- function(key)
  if (case_is_sprint(key)) sprint_case(key, dt = 0.05) else gastin_case(key, dt = 0.25)

case_vo2max <- function(key)
  if (case_is_sprint(key)) sprint_case_vo2max(key) else gastin_case_vo2max(key)

case_label <- function(key) {
  all <- c(GASTIN_CASE_NAMES, SPRINT_CASE_NAMES)
  i <- match(key, all)
  if (is.na(i)) key else names(all)[i]
}