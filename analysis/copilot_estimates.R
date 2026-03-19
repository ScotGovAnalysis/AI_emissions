# run_scenarios.R
# -----------------------------------------------------------------------------
# Script to compute environmental impacts for scenarios using GPT‑5
# -----------------------------------------------------------------------------

require(tidyverse)
require(here)
# -------------------------------------------------------------------------
# Load data produced by global.R
# -------------------------------------------------------------------------
# source("../app/global.R")  # creates object: dat
# source("../R/functions.R")
source(here("app", "global.R"))
source(here("R", "functions.R"))
# -------------------------------------------------------------------------
# Load scenario definitions
# -------------------------------------------------------------------------
# scenarios <- read_csv("../data/scenarios.csv", show_col_types = FALSE)
scenarios <- read_csv(here("data", "scenarios.csv"), show_col_types = FALSE)



# =============================================================================
# EXECUTION: Compute results for all scenarios
# =============================================================================

scenario_results <- scenarios |>
  rowwise() |>
  mutate(
    proportions = list(compute_proportion_table(pick(everything()))),
    joined = list(join_with_global(proportions)),
    results = list(compute_environmental_totals(joined, number_prompts))
  ) |>
  unnest(results) |>
  select(
    scenario,
    energy_mwh,
    carbon_tons,
    water_kl
  )

# Write output if desired:
# write_csv(scenario_results, "scenario_results.csv")

# print(scenario_results)
