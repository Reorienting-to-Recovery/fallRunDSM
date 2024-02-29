library(tidyverse)
library(fallRunDSM)
library(plotly)
library(producePMs)

# Source helper functions
# source("data-raw/helper_graph_functions.R")
# View prelim R2R results

# seed
# BASELINE --
new_params <- fallRunDSM::r_to_r_baseline_params
new_params$movement_hypo_weights <- c(1, rep(0, 7))

# seed
r2r_seeds <- fallRunDSM::fall_run_model(mode = "seed",
                                        seeds = fallRunDSM::adult_seeds,
                                        ..params =  new_params,
                                        delta_surv_inflation = FALSE)
r2r_seeds$adults
# run model
r2r_model_results <- fallRunDSM::fall_run_model(mode = "simulate",
                                                ..params =  new_params,
                                                seeds = r2r_seeds,
                                                delta_surv_inflation = FALSE)


r2r_model_results$spawners
non_spawn_regions <- c("Upper-mid Sacramento River", "Sutter Bypass",
                       "Lower-mid Sacramento River", "Yolo Bypass",
                       "Lower Sacramento River", "San Joaquin River"
                       # "American River"
                       # "Feather River"
) # remove american river (hab too high)

spawn <- dplyr::as_tibble(r2r_model_results$spawners) |> #change which results to look at diff plots
  dplyr::mutate(location = fallRunDSM::watershed_labels) |>
  tidyr::pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
  dplyr::filter(!location %in% non_spawn_regions) |>
  dplyr::group_by(year,
           location
  ) |>
  dplyr::summarize(total_spawners = sum(spawners, na.rm = TRUE)) |>
  dplyr::mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners,
             color = location
  )) +
  geom_line() +
  theme_minimal() +
  labs(y = "Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

ggplotly(spawn)


# CHECK against grandtab
grandtab_totals <- dplyr::as_tibble(DSMCalibrationData::grandtab_observed$fall)|> #change which results to look at diff plots
  dplyr::mutate(location = fallRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1998`:`2017`), values_to = 'spawners', names_to = "year") %>%
  filter(!location %in% non_spawn_regions) |>
  group_by(year,
           # location
  ) |>
  summarize(total_spawners = sum(spawners, na.rm = TRUE)) |>
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners,
             # color = location
  )) +
  geom_line() +
  theme_minimal() +
  labs(y = "Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

ggplotly(grandtab_totals)


# IND POP CHECK
# TODO questions for technical team
# - Intrinsic growth rate? that is what it states in phase 1 report, performs worse than current strategy
#
# Recovery Rules
# Above 500 spawners for at least 5 of last 10 years
# Phos < 5% for last 5 years
# average growth rate above 0 for full scenario
# geometric mean crr above 1 for full scenario
#
results_df <- create_model_results_dataframe(r2r_model_results,
                                             scenario = "kitchen sink",
                                             model_parameters = fallRunDSM::r_to_r_kitchen_sink_params, selected_run = "fall")
potential_dependent_pops <- c("Bear River", "Big Chico Creek", "Elder Creek", "Paynes Creek",  "Stoney Creek", "Thomes Creek")

ind_pops <- results_df |>
  select(-size_or_age, -origin, -month, -run) |>
  filter(performance_metric %in% c("Natural Spawners", "2.2 Growth Rate Spawners",
                                   "4 PHOS", "2.1 CRR: Total Adult to Returning Natural Adult"),
         !location %in% potential_dependent_pops,
         !location %in% non_spawn_regions) |>
  pivot_wider(names_from = performance_metric, values_from = value) |>
  # round results
  mutate(`2.1 CRR: Total Adult to Returning Natural Adult` = round(`2.1 CRR: Total Adult to Returning Natural Adult`, 1),
         intrinsic_growth_rate = round(log(`Natural Spawners`) - log(lag(`Natural Spawners`))),
         `2.2 Growth Rate Spawners` = round(`2.2 Growth Rate Spawners`, 1),
         `4 PHOS` = round(`4 PHOS`, 2),
         `Natural Spawners` = round(`Natural Spawners`)) |>
  # categorize as meets threshold or not
  mutate(above_500_spawners = if_else(`Natural Spawners` > 500, TRUE, FALSE),
         phos_less_than_5_percent = ifelse(`4 PHOS` < .05, TRUE, FALSE),
         crr_above_1 = ifelse(`2.1 CRR: Total Adult to Returning Natural Adult` >= 1, TRUE, FALSE),
         # intrinskic_growth_rate_above_1 = ifelse(intrinsic_growth_rate >= 1, TRUE, FALSE),
         growth_rate_above_1 = ifelse(`2.2 Growth Rate Spawners` >= 0, TRUE, FALSE),
         independent_population = ifelse(above_500_spawners & phos_less_than_5_percent &
                                           growth_rate_above_1 & crr_above_1, TRUE, FALSE))

glimpse(ind_pops)

# Review Pop and Hatchery Biological Parameter,
# Must be met each year
ind_pops |>
  select(year, location, above_500_spawners, phos_less_than_5_percent,) |>
  pivot_longer(above_500_spawners:phos_less_than_5_percent, names_to = "metric", values_to = "value") |>
  # filter(year <= 17, year > 1) |> # removes last three years because CRR is NA,
  # first year because growth rate is NA
  ggplot(aes(x = year, y = location, color = value)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("azure2", "cadetblue4", "#CCC591"), name = "") +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Pouplation and Hatchery Biological Parameters") +
  theme(legend.position = "bottom") +
  facet_wrap(~metric)

# Review Productivity Metrics
# Must be met on average over 20 year simulation at each tributary
# Where we fall short - Calveras, Feather (both super close to one)
ind_pops |>
  filter(year > 5) |>
  group_by(location) |>
  summarise("Average CRR" = round(mean(`2.1 CRR: Total Adult to Returning Natural Adult`, na.rm = TRUE), 2),
            "Average Growth Rate" = round(mean(`2.2 Growth Rate Spawners`, na.rm = TRUE), 2)) |>
  pivot_longer("Average CRR":"Average Growth Rate", names_to = "stat", values_to = "value") |>
  mutate(above_threshold = case_when(stat == "Average CRR" & value > 1 ~ TRUE,
                                     stat ==  "Average Growth Rate" & value > 0 ~ TRUE,
                                     T ~ FALSE)) |>
  ggplot(aes(x = location, y = value, fill = above_threshold)) +
  geom_col(size = 4) +
  scale_fill_manual(values = c("#CCC591", "cadetblue4"), name = "") +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Average Productivity Metrics (yr 5 - 20) of simulation") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~stat, nrow = 2)

ind_pops |>
  filter(year <= 17, year > 1) |> # removes last three years because CRR is NA,
  # first year because growth rate is NA
  ggplot(aes(x = year, y = location, color = independent_population)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("azure2", "cadetblue4", "#CCC591"), name = "") +
  theme_minimal() +
  labs(y = "",
       x = "",
       title = "Independent Populations") +
  theme(legend.position = "bottom")


water_yt_lookup <- waterYearType::water_year_indices |>
  filter(WY > 1977, WY < 2001, location == "Sacramento Valley") |>
  mutate(water_year = WY,
         year_type = Yr_type) |> glimpse()

year_lookup <- tibble(year = 1:21,
                      actual_year = 1980:2000)

flow_spawn_plot_data <- results_df |>
  filter(performance_metric == "1 All Spawners") |>
  group_by(year, scenario, run) |>
  summarize(value = sum(value, na.rm = TRUE)) |>
  left_join(year_lookup) |>
  mutate(date = as.Date(paste0(actual_year, "-12-31"))) |>
  filter(scenario != "No Hatchery") |> glimpse()

scenario_six_colors <- c("#02401B", "#9A8822", "#798E87", "#5B1A18","#972D15", "#DC863B", "#AA9486")

ggplot() +
  geom_line(data = flow_spawn_plot_data, aes(x = date, y = value, color = scenario),linewidth = .5, alpha = 1) +
  scale_color_manual(values = scenario_six_colors) +
  # geom_line(data = flow_spawn_plot_data |> filter(scenario == "Baseline"), aes(x = date, y = value), color = "black", linewidth = .5, alpha = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Spawner Abundance over Time",
       y = "Spawner Abundance",
       x = "Year",
       color = "Scenario Name",
       fill = "Hydrology",
       #  caption = "Note: These numbers only reflect Upper Sacramento River Fall Run Chinook Spawners. Baseline and No Hatchery perform very similarly in the Upper Sacramento River."
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),# move caption to the left
    legend.position = c(0.85, 0.7),
    # legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20)
  )


produce_spawner_abundance_pm(results_df)
produce_crr_geometric_mean_pm <- function(model_results_df){
  geom_mean_calc <- function(watershed, scenario) {
    data <- model_results_df |>
      filter(performance_metric == "2.1 CRR: Total Adult to Returning Natural Adult",
             location == watershed) |>
      select(year, location, scenario, run, performance_metric, value) |>
      mutate(geometric_mean = zoo::rollapply(value, 3, psych::geometric.mean, fill = NA)) |>
      filter(!is.na(geometric_mean))
    return(data)
  }
  watersheds <- rep(fallRunDSM::watershed_labels, 7)
  scenarios_lists <- c(rep("Baseline", 31),
                       rep("Theoretical Max Habitat", 31),
                       rep("No Harvest", 31),
                       rep("No Hatchery", 31),
                       rep("Max Flow", 31),
                       rep("Max Flow & Max Habitat", 31),
                       rep("Max Hatchery", 31))

  res <- purrr::map2(watersheds,scenarios_lists, geom_mean_calc) |> reduce(bind_rows)
  goem_mean_crr <- res |>
    group_by(location, scenario) |>
    summarize(average_crr = mean(geometric_mean, na.rm = TRUE)) |>
    ungroup() |>
    mutate(average_crr = ifelse(average_crr == Inf, 0, average_crr)) |>
    group_by(scenario) |>
    summarize(avg_annual_crr = mean(average_crr, na.rm = T),
              min_annual_crr = min(average_crr, na.rm = T),
              max_annual_crr = max(average_crr, na.rm = T))
  return(goem_mean_crr)
}
produce_crr_geometric_mean_pm(results_df)
produce_growth_rate_pm(results_df)

