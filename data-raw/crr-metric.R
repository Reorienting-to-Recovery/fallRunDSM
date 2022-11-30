library(tidyverse)
library(fallRunDSM)

s <- fall_run_model(mode = "seed")
sim <- fall_run_model(seeds = s, mode = "simulate")


# adult return ratio (simple approach) --------------------------------

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
  ggplot(aes( sim_year, total_return)) + geom_point() +
  scale_x_continuous(breaks = 1:20)

# Adult return ratio (naiive approach) ------------------------------------
# # Adult return to natural origin/spawning adult ratio (mean) is
# defined as: the number of natural origin adult returns in year
# X + 3 divided by the number of in-river spawners in year X.

# The proportion of natural spawners created by in river spawners from three years prior

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
  group_by(watershed) |>
  mutate(
    year = as.numeric(year),
    lag_total = lag(total_spawners, 3),
    origin_year = lag(year, 3),
    metric = natural_spawners / lag_total) |> glimpse()

sim_spawners |>
 # filter(watershed == "Upper Sacramento River") |>
  ggplot(aes(origin_year, metric)) +
  geom_line(aes(color = watershed)) +
  scale_x_continuous(breaks = 1:20)

# 20 year average across all watersheds
sim_spawners |>
  group_by(watershed) |>
  summarise(
    mean_adult_return_to_origin = mean(metric, na.rm = TRUE),
    median_adult_return_to_origin = median(metric, na.rm = TRUE)
  )


# Juveniles metric ---------------------------------------------------------------
# Juvenile to adult ratio: Juveniles year 1/natural origin adults returning in year 4
# the juvenile to adult return ration (mean) is defined as the number of juveniles produced in year X
# divided by the number of natural origin adult returns in year X +3.


## Juveniles naiive approach
# The percent of returning adults that a cohort of juveniles produces (metric_rev)
# total juveniles produced is assumed to be the value before survival and migration is applied
juveniles <- sim$juveniles |> # TODO: what point in the model do we want to capture total juveniles?
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

juveniles |>
  group_by(watershed) |>
  summarise(avg_juvenile_metric_perc = mean(metric_rev, na.rm = TRUE) * 100) |>
  arrange(-avg_juvenile_metric_perc)

juveniles |>
 # filter(watershed == "Upper Sacramento River") |>
  ggplot(aes( year, metric_rev)) +
  geom_line(aes(color = watershed)) +
  #geom_point() +
  scale_x_continuous(breaks = 1:20)







