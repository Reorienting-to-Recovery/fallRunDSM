library(tidyverse)

# View prelim R2R results
set.seed(123119)
r2r_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)

r2r_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
                                                seeds = r2r_seeds)

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
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

# CCR plot
natural_spawners <- (r2r_model_results$proportion_natural * r2r_model_results$spawners) |>
  as_tibble() |>
  mutate(watershed = fallRunDSM::watershed_labels) |>
  pivot_longer(names_to = "year", values_to = "natural_spawners", -watershed)

juveniles <- r2r_model_results$juveniles |>
  as_tibble() |>
  mutate(year = as.numeric(year)) |>
  group_by(year, watershed) |>
  summarise(total_juveniles = sum(juveniles)) |>
  ungroup() |>
  left_join(natural_spawners |>
              mutate(year = as.numeric(year))) |>
  arrange(watershed, year) |>
  group_by(watershed) |>
  mutate(natural_spawners_lag = lead(natural_spawners, 3),
         metric = total_juveniles / natural_spawners_lag,
         metric_rev = natural_spawners_lag / total_juveniles) |>
  ungroup()

juv_plot <- juveniles |>
  group_by(year) |>
  summarize(mean_crr = mean(metric_rev, na.rm = T)) |>
  ggplot(aes( year, mean_crr)) +
  geom_line() +
  scale_x_continuous(breaks = 1:20) +
  ylab('Adult Returns Per Juveniles') +
  xlab("Year") +
  theme_minimal() +
  theme(text = element_text(size = 20))

juv_plot

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

