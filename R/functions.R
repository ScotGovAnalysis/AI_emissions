# =============================================================================
# FUNCTIONS
# =============================================================================

#' Compute model × length proportions from scenario definitions
#'
#' @param row Single-row tibble from scenarios.csv
#'
#' @return Tibble with model, length, final_prop
#' @export
compute_proportion_table <- function(row) {
  
  # Base length proportions
  base_len <- tibble(
    length = c("Long", "Medium", "Short"),
    base_prop = c(
      row$long,
      row$medium,
      1 - row$long - row$medium
    )
  )
  
  # Complexity proportions within each length
  complexity <- tibble(
    length = c("Long", "Medium", "Short"),
    Complex = c(row$l_c, row$m_c, row$s_c),
    Medium  = c(row$l_m, row$m_m, row$s_m),
    Simple  = c(
      1 - row$l_c - row$l_m,
      1 - row$m_c - row$m_m,
      1 - row$s_c - row$s_m
    )
  )
  
  # Model-to-complexity mapping
  model_map <- tibble(
    model = c("GPT-5 (high)", "GPT-5 (medium)", "GPT-5 (low)"),
    complexity_label = c("Complex", "Medium", "Simple")
  )
  
  # Join and compute final proportions
  base_len |>
    left_join(complexity, by = "length") |>
    tidyr::pivot_longer(
      cols = c(Complex, Medium, Simple),
      names_to = "complexity_label",
      values_to = "complexity_prop"
    ) |>
    left_join(model_map, by = "complexity_label") |>
    filter(!is.na(model)) |>
    mutate(final_prop = base_prop * complexity_prop) |>
    select(model, length, final_prop)
}

#' Join scenario proportions with GPT‑5 environmental data
#'
#' @param proportions Tibble from compute_proportion_table()
#'
#' @return Tibble joined with dat (global GPT‑5 environmental averages)
#' @export
join_with_global <- function(proportions) {
  dat |>
    left_join(proportions, by = c("model", "length")) |>
    rename(prop = final_prop)
}


#' Compute total environmental impact for a scenario
#'
#' @param dat_joined Tibble produced by join_with_global()
#' @param total_prompts Total number of prompts for the scenario
#'
#' @return Tibble with total energy (MWh), emissions (tonnes), water (kL)
#' @export
compute_environmental_totals <- function(dat_joined, total_prompts) {
  dat_joined |>
    mutate(
      energy = mean_combined_energy_wh * prop * total_prompts,
      carbon = mean_combined_carbon_g_co2e * prop * total_prompts,
      water  = mean_combined_water_site_source_m_l * prop * total_prompts
    ) |>
    summarise(
      energy_mwh  = sum(energy) / 1e6,
      carbon_tons = sum(carbon) / 1e6,
      water_kl    = sum(water) / 1e6
    )
}