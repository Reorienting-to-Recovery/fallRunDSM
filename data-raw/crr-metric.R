library(tidyverse)
library(zoo)
class(f$spawners)

f <- fall_run_model(seeds = s, mode = "simulate")

raw_adults <- f$raw_adults

adults <- raw_adults |>
  as_tibble() |>
  mutate(watershed = fallRunDSM::watershed_labels) |>
  pivot_longer(names_to = "year", values_to = "adults", V1:V25) |>
  mutate(year = readr::parse_number(year),
         cal_year = year + 1979)

zoo::rollapply()

adults |>
  filter(watershed == "Upper Sacramento River") |>
  mutate(l2 = lag(adults * .25, 2),
         l3 = lag(adults * .50, 3),
         l4 = lag(adults * .25, 4),
         lag_sums = l2 + l3 + l4,
         crr = adults/lag_sums) |>
  View()



ret_adults <- f$returning_adults

ret_adults |>
  mutate(return_year = year + year_return) |>
  filter(watershed == "Upper Sacramento River")
