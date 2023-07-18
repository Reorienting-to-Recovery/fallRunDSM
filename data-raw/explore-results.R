library(tidyverse)
library(fallRunDSM)
library(plotly)
# Source helper functions
source("data-raw/helper_graph_functions.R")
# View prelim R2R results

# seed
r2r_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)

# run model
r2r_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
                                                seeds = r2r_seeds)
r2r_model_results$spawners
  r2r_model_results$proportion_natural_at_spawning


spawn <- dplyr::as_tibble(r2r_model_results$spawners) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
  group_by(year, location) |>
  summarize(total_spawners = sum(spawners)) |>
  # filter(location != "Feather River") |>
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners, color = location)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

spawn


# seed
r2r_seeds_max_flow_tmh <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_max_flow_max_hab_params)

# run model
r2r_model_results_max_flow_tmh <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_max_flow_max_hab_params,
                                                seeds = r2r_seeds_max_flow_tmh)

# seed
r2r_seeds_max_flow <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_max_flow_params)

# run model
r2r_model_results_max_flow <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_max_flow_params,
                                                             seeds = r2r_seeds_max_flow)

max_flow_tmh <- dplyr::as_tibble(r2r_model_results_max_flow_tmh$spawners) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels,
                run = "Max Flow Max Hab") |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
  group_by(year, run) |>
  summarize(total_spawners = sum(spawners))

dplyr::as_tibble(r2r_model_results_max_flow$spawners) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels,
                run = "Max Flow") |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
  group_by(year, run) |>
  summarize(total_spawners = sum(spawners)) |>
  bind_rows(max_flow_tmh) |>
  # filter(location != "Feather River") |>
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners, color = run)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

spawn


dplyr::as_tibble(r2r_model_results$spawners) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
  group_by(year) |>
  summarize(total_spawners = sum(spawners)) |>
  # filter(location != "Feather River") |>
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

r2r_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)

r2r_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_max_flow_max_hab_params,
                                                seeds = r2r_seeds)

# BASIC ABUNDANCE PLOTS


# Total Spawners (natural and hatchery )
plot_total_spawners(r2r_model_results, "Total Spawners")

# Single watershed total spawners
plot_single_watershed_natural_spawners(model_results = r2r_model_results,
                                       watershed = "Battle Creek",
                                       result_type = "Total Spawners")

# All watershed
plot_all_watersheds_spawners(model_results = r2r_model_results,
                             result_type = "Total Spawners")
plot_all_watersheds_spawners(model_results = r2r_model_results,
                             result_type = "Total Natural Spawners")
# CCR Results ------------------------------------------------------------------
# Adult to adult metric --------------------------------------------------------
# Data processing
# TODO these show an example where we just plot the mean - the adult to adult plots
# show an example where we plot each watershed. Confirm which route we want to go?
plot_juv_crr(model_results = r2r_model_results,
               result_type = "Total Spawners")

plot_juv_crr(model_results = r2r_model_results,
             result_type = "Total Natural Spawners")
## CCR adults to adults --------------------------------------------------------
# data processing
# TODO confirm we do not want total spawn and natural spawn compared against each other
plot_adult_crr(model_results = r2r_model_results,
               result_type = "Total Spawners")

plot_adult_crr(model_results = r2r_model_results,
               result_type = "Total Natural Spawners")

# Habitat percentages by watershed ---------------------------------------------
produce_habitat_ratios(model_parameters = r_to_r_baseline_params, watershed = "Yuba River")
produce_habitat_ratios(model_parameters = r_to_r_baseline_params, watershed = "Lower Sacramento River")


### OG MODEL COMPARISON ########################################################
# OG model results
set.seed(123119)
seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::params)

model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::params, seeds = seeds)

plot_total_natural_spawners(model_results, "Total Natural Spawners")
