library(tidyverse)

# View prelim R2R results
set.seed(123119)
r2r_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)

r2r_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params, seeds = r2r_seeds)

r22_baseline_nat_spawn <- dplyr::as_tibble(r2r_model_results$spawners * r2r_model_results$proportion_natural) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels)

# Total natural spawners plot
r22_baseline_nat_spawn %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  group_by(year) |>
  summarize(total_spawners = sum(natural_spawners)) |>
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Total Natural Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma)

# OG model results
set.seed(123119)
seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::params)

model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::params, seeds = seeds)

nat_spawn <- dplyr::as_tibble(model_results$spawners * model_results$proportion_natural) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels)

nat_spawn %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, natural_spawners, color = location)) +
  geom_line() +
  theme_minimal()

