library(tidyverse)
library(fallRunDSM)

s <- fall_run_model(mode = "seed")
sim <- fall_run_model(seeds = s, mode = "simulate")

returning_adults <- sim$returning_adults

crr_natural_adults <- returning_adults |>
  mutate(crr = return_total_nat / natural_adults)

View(crr_natural_adults)
crr_natural_adults |>
  group_by(watershed, sim_year) |>
  summarise(
    total_return = sum(crr)
  ) |>
  filter(watershed == "Upper Sacramento River") |>
  ggplot(aes( sim_year, total_return)) + geom_point()


returning_adults |> View()



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


# Adult return to natural origin/spawning adult ratio (mean) is
# defined as: the number of natural origin adult returns in year
# X + 3 divided by the number of in-river spawners in year X.










