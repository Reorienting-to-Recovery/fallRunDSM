scenario_six_colors <- c("#02401B", "#9A8822", "#798E87", "#5B1A18","#972D15", "#DC863B", "#AA9486")

library(fallRunDSM)
library(tidyverse)
# BASELINE --
new_params <- fallRunDSM::r_to_r_baseline_params
new_params$movement_hypo_weights <- c(1, rep(0, 7))

# KS
new_params_ks <- fallRunDSM::r_to_r_kitchen_sink_params
new_params_ks$movement_hypo_weights <- c(1, rep(0, 7))

# H&H
new_params_hh <- fallRunDSM::r_to_r_habitat_and_hatchery_params
new_params_hh$movement_hypo_weights <- c(1, rep(0, 7))

# DRY YEARS
new_params_dy <- fallRunDSM::r_to_r_dry_years_params
new_params_dy$movement_hypo_weights <- c(1, rep(0, 7))


# seed
r2r_seeds <- fallRunDSM::fall_run_model(mode = "seed",
                                           seeds = fallRunDSM::adult_seeds,
                                           ..params =  new_params, # seed with baseline, then kitchen sink
                                           delta_surv_inflation = FALSE)

# run model baseline
r2r_model_results_baseline <- fallRunDSM::fall_run_model(mode = "simulate",
                                                         ..params =  new_params,
                                                         seeds = r2r_seeds,
                                                         delta_surv_inflation = FALSE)

# run model ks
r2r_model_results_ks <- fallRunDSM::fall_run_model(mode = "simulate",
                                                   ..params =  new_params_ks,
                                                   seeds = r2r_seeds,
                                                   delta_surv_inflation = TRUE)


# run model hh
r2r_model_results_hh <- fallRunDSM::fall_run_model(mode = "simulate",
                                                   ..params =  new_params_hh,
                                                   seeds = r2r_seeds,
                                                   delta_surv_inflation = FALSE)

# run model dy
r2r_model_results_dy <- fallRunDSM::fall_run_model(mode = "simulate",
                                                   ..params =  new_params_dy,
                                                   seeds = r2r_seeds,
                                                   delta_surv_inflation = TRUE)
non_spawn_regions <- c("Upper-mid Sacramento River", "Sutter Bypass",
                       "Lower-mid Sacramento River", "Yolo Bypass",
                       "Lower Sacramento River", "San Joaquin River")
# baseline spawn totals
spawn_baseline <- suppressMessages(dplyr::as_tibble(r2r_model_results_baseline$spawners) |> #change which results to look at diff plots
                                     dplyr::mutate(location = fallRunDSM::watershed_labels) |>
                                     pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
                                     filter(!location %in% non_spawn_regions) |>
                                     group_by(year) |>
                                     summarize(total_spawners = sum(spawners, na.rm = TRUE)) |>
                                     mutate(year = as.numeric(year),
                                            scenario = "Baseline")) |>
  ungroup()

# baseline spawn totals
spawn_ks <- suppressMessages(dplyr::as_tibble(r2r_model_results_ks$spawners) |> #change which results to look at diff plots
                                     dplyr::mutate(location = fallRunDSM::watershed_labels) |>
                                     pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
                                     filter(!location %in% non_spawn_regions) |>
                                     group_by(year) |>
                                     summarize(total_spawners = sum(spawners, na.rm = TRUE)) |>
                                     mutate(year = as.numeric(year),
                                            scenario = "Kitchen Sink")) |>
  ungroup()

# baseline spawn totals
spawn_hh <- suppressMessages(dplyr::as_tibble(r2r_model_results_hh$spawners) |> #change which results to look at diff plots
                                     dplyr::mutate(location = fallRunDSM::watershed_labels) |>
                                     pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
                                     filter(!location %in% non_spawn_regions) |>
                                     group_by(year) |>
                                     summarize(total_spawners = sum(spawners, na.rm = TRUE)) |>
                                     mutate(year = as.numeric(year),
                                            scenario = "Habitat and Hatchery"))
# baseline spawn totals
spawn_dy <- suppressMessages(dplyr::as_tibble(r2r_model_results_dy$spawners) |> #change which results to look at diff plots
                                     dplyr::mutate(location = fallRunDSM::watershed_labels) |>
                                     pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
                                     filter(!location %in% non_spawn_regions) |>
                                     group_by(year) |>
                                     summarize(total_spawners = sum(spawners, na.rm = TRUE)) |>
                                     mutate(year = as.numeric(year),
                                            scenario = "Dry Years")) |>
  ungroup()


# create percent diff formula
percent_diff <- function(old_value, new_value) {
  ((new_value - old_value) / old_value) * 100
}

# create spawner
all_spawners <- bind_rows(spawn_baseline,
                          spawn_dy,
                          spawn_hh,
                          spawn_ks) |> glimpse()


# View(spawners)
blended_scenario_plot <- all_spawners |>
  ggplot() +
  geom_line(aes(x = year, y = total_spawners, color = scenario),linewidth = .5, alpha = 1) +
  scale_color_manual(values = scenario_six_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Spawner Abundance over Time",
       y = "Spawner Abundance",
       x = "Year",
       color = "Bookend Scenario",
       fill = "Hydrology",
       #  caption = "Note: These numbers only reflect Upper Sacramento River Fall Run Chinook Spawners. Baseline and No Hatchery perform very simmilarly in the Upper Sacramento River."
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = "top",
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )

ggsave("data-raw/figures/blended_scenario_plot.png")
