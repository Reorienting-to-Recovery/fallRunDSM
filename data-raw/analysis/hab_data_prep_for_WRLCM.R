# Prep results for Ann Marie
# SPAWN
baseline <- DSMhabitat::fr_spawn$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
max_eff <- DSMhabitat::fr_spawn$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
spawn <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels,
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) |>
  arrange(year, month, watershed) %>%
  mutate(
    baseline = as.vector(baseline),
    max_eff = as.vector(max_eff),
    habitat_type = "spawning") |>
  filter(watershed %in% c("Upper Sacramento River"))


spawn %>%
  filter(month %in% c(10:12)) |>
  transmute(watershed, date = ymd(paste(year, month, 1)), baseline, max_eff) %>%
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_point() +
  facet_wrap(~watershed, scales = 'free_y')

# FP
baseline <- DSMhabitat::fr_fp$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
max_eff <- DSMhabitat::fr_fp$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels,
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |>
  arrange(year, month, watershed) %>%
  mutate(
    baseline = as.vector(baseline),
    max_eff = as.vector(max_eff),
    habitat_type = "floodplain rearing") |>
  filter(watershed %in% c("Upper Sacramento River", "Upper-mid Sacramento River",
                          "Lower-mid Sacramento River", "Lower Sacramento River"))

fp %>%
  filter(month %in% c(1:7)) |>
  transmute(watershed, date = ymd(paste(year, month, 1)), baseline, max_eff) %>%
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_point() +
  facet_wrap(~watershed, scales = 'free_y')

# Rearing Juv
baseline <- DSMhabitat::fr_juv$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
max_eff <- DSMhabitat::fr_juv$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels,
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |>
  arrange(year, month, watershed) %>%
  mutate(
    baseline = as.vector(baseline),
    max_eff = as.vector(max_eff),
    habitat_type = "juvenile rearing") |>
  filter(watershed %in% c("Upper Sacramento River", "Upper-mid Sacramento River",
                          "Lower-mid Sacramento River", "Lower Sacramento River"))

juv %>%
  filter(month %in% c(1:7), year %in% (1985:1990)) |>
  transmute(watershed, date = ymd(paste(year, month, 1)), baseline, max_eff) %>%
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_point() +
  facet_wrap(~watershed, scales = 'free_y')

# Rearing Fry
baseline <- DSMhabitat::fr_fry$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
max_eff <- DSMhabitat::fr_fry$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels,
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |>
  arrange(year, month, watershed) %>%
  mutate(
    baseline = as.vector(baseline),
    max_eff = as.vector(max_eff),
    habitat_type = "fry rearing") |>
  filter(watershed %in% c("Upper Sacramento River", "Upper-mid Sacramento River",
                          "Lower-mid Sacramento River", "Lower Sacramento River"))

fry %>%
  transmute(watershed, date = ymd(paste(year, month, 1)), baseline, max_eff) %>%
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free_y')

baseline_north_delta <- DSMhabitat::delta_habitat$r_to_r_baseline[,,1] %>% DSMhabitat::square_meters_to_acres()
baseline_south_delta <- DSMhabitat::delta_habitat$r_to_r_baseline[,,2] %>% DSMhabitat::square_meters_to_acres()

max_eff_north_delta <- DSMhabitat::delta_habitat$r_to_r_tmh[,,1] %>% DSMhabitat::square_meters_to_acres()
max_eff_south_delta <- DSMhabitat::delta_habitat$r_to_r_tmh[,,2] %>% DSMhabitat::square_meters_to_acres()

delta <- expand_grid(
  month = 1:12,
  year = 1980:2000) |>
  arrange(year, month) %>%
  mutate(
    baseline_north = as.vector(baseline_north_delta),
    baseline_south = as.vector(baseline_south_delta),
    max_eff_north = as.vector(max_eff_north_delta),
    max_eff_south = as.vector(max_eff_south_delta),
    habitat_type = "delta rearing") |>
  pivot_longer(baseline_north:max_eff_south, names_to = "scenario_location", values_to = "habitat") |>
  mutate(scenario = ifelse(scenario_location %in% c("baseline_north", "baseline_south"), "baseline", "max_eff"),
         watershed = ifelse(scenario_location %in% c("baseline_north", "max_eff_north"), "North Delta", "South Delta")) |>
  select(-scenario_location) |>
  pivot_wider(names_from = scenario, values_from = habitat) |> glimpse()

delta %>%
  transmute(watershed, date = ymd(paste(year, month, 1)), baseline, max_eff) %>%
  gather(version, acres, -watershed, -date)  %>%
  ggplot(aes(date, acres, color = version)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free_y')

# No change in bypass habitat

habitat_baseline_to_kitchen_sink_scaling <- bind_rows(spawn, fp, juv, fry, delta) |>
  mutate(kitchen_sink_scaling = ifelse(baseline == 0 & max_eff ==0, 0, (max_eff/baseline))) |>
  rename(kitchen_sink = max_eff) |>
  glimpse()

# unique(round(habitat_baseline_to_kitchen_sink_scaling$test) == round(habitat_baseline_to_kitchen_sink_scaling$kitchen_sink))

habitat_baseline_to_kitchen_sink_scaling |>
  # filter(habitat_type == "spawning" & month %in% c(10:12)) |>
  ggplot(aes(x = kitchen_sink_scaling, fill = watershed)) +
  geom_histogram() +
  facet_wrap(~habitat_type, scales = "free_x") +
  theme_minimal()

write_csv(habitat_baseline_to_kitchen_sink_scaling, "habitat_baseline_to_kitchen_sink_scaling.csv")

DSMhabitat::fr_fp
DSMhabitat::fr_fry
DSMhabitat::fr_juv
DSMhabitat::fr_spawn
