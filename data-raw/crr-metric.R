library(tidyverse)
library(fallRunDSM)

s <- fall_run_model(mode = "seed")
sim <- fall_run_model(seeds = s, mode = "simulate")

sim$adults_in_ocean[, 1]
10577 * c(.25, .5, .25)

natural_spawners <- (sim$proportion_natural * sim$spawners) |>
  as_tibble() |>
  mutate(watershed = fallRunDSM::watershed_labels) |>
  pivot_longer(names_to = "year", values_to = "natural_spawners", -watershed)

in_river_spawners <- sim$spawners |>
  as_tibble() |>
  mutate(watershed = fallRunDSM::watershed_labels) |>
  pivot_longer(names_to = "year", values_to = "total_spawners", -watershed)


sim_spawners <- in_river_spawners |>
  left_join(natural_spawners) |>
  mutate(lag_total = lag(total_spawners, 3),
         metric = natural_spawners / lag_total)



