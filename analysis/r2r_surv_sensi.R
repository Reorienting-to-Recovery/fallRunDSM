# Sensitivity Analysis for R2R
# Test out change in model results given 5, 10, and 20% increases in ocean entry surv and prespawn surv

library(tidyverse)
library(fallRunDSM)

sensi_model_run <- function(sensi_type, sensi_quantity) {
  sensi_seeds <- fallRunDSM::fall_run_model(mode = "seed",
                                        ..params = fallRunDSM::r_to_r_baseline_params,
                                        r_to_r_sensi = sensi_type,
                                        sensi_increase = sensi_quantity)

  sensi_model_run <- fallRunDSM::fall_run_model(mode = "simulate",
                                                ..params = fallRunDSM::r_to_r_baseline_params,
                                                seeds = sensi_seeds,
                                                r_to_r_sensi = sensi_type,
                                                sensi_increase = sensi_quantity)
  output <- dplyr::as_tibble(sensi_model_run$spawners * sensi_model_run$proportion_natural) |>
    dplyr::mutate(location = fallRunDSM::watershed_labels,
                  survival_type = sensi_type,
                  survival_scaling = sensi_quantity) |>
    dplyr::select(location, survival_type, survival_scaling,  `1`:`20`)

  return(output)
}


res <- sensi_model_run(sensi_type = "rearing habitat", sensi_quantity = 1)

sensi_types <- c(
  # "prespawn surv", "prespawn surv", "prespawn surv","prespawn surv",
                 "spawning habitat", "spawning habitat", "spawning habitat", "spawning habitat",
                 # "spawning and ocean", "spawning and ocean","spawning and ocean","spawning and ocean",
                 # "prespawn and ocean", "prespawn and ocean", "prespawn and ocean", "prespawn and ocean",
                 # "ocean surv", "ocean surv", "ocean surv", "ocean surv",
                 "rearing habitat", "rearing habitat", "rearing habitat", "rearing habitat",
                 "rearing and spawning habitat","rearing and spawning habitat",
                 "rearing and spawning habitat", "rearing and spawning habitat")
sensi_quantity <- c(rep(c(1.05, 1.1, 1.2, 1), 3))

model_results_2 <- purrr::map2(sensi_types, sensi_quantity, sensi_model_run) |>
  purrr::reduce(bind_rows)
View(model_results_2)
#
saveRDS(model_results_2, "analysis/r_2_r_sensi_results_habitat.rds")
# Total natural spawners plot
# color_pal <- c("#9A8822",  "#F8AFA8", "#FDDDA0", "#74A089", "#899DA4", "#446455", "#DC863B", "#C93312")

color_pal <- c("#9A8822", "#74A089", "#446455", "#DC863B", "#C93312")
model_results_2 <- readRDS("analysis/r_2_r_sensi_results_habitat.rds")
model_results <- readRDS("analysis/r_2_r_sensi_results.rds")
nat_spawn_results <- model_results %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year")  |>
  mutate(survival_scaling = case_when(survival_scaling == 1.05 ~ "5% increase",
                                      survival_scaling == 1.10 ~ "10% increase",
                                      survival_scaling == 1.20 ~ "20% increase",
                                      survival_scaling == 1 ~ "Baseline - No Increase"),
         survival_type = case_when(survival_type == "prespawn surv" ~ "Prespawn Survival",
                                   survival_type == "ocean surv" ~ "Ocean Entry Survival",
                                   survival_type == "spawning habitat" ~ "Spawning Habitat",
                                   survival_type == "spawning and ocean" ~ "Spawning Habitat and Ocean Entry Survival",
                                   survival_type == "prespawn and ocean" ~ "Prespawn Survival and Ocean Entry Survival",
                                   survival_type == "rearing habitat" ~ "Rearing Habitat",
                                   survival_type == "rearing and spawning habitat" ~ "Spawning and Rearing Habitat")) |>
  group_by(year, survival_type, survival_scaling) |>
  summarize(total_spawners = sum(natural_spawners)) |>
  mutate(year = as.numeric(year),
         is_baseline = ifelse(survival_scaling == "Baseline - No Increase", TRUE, FALSE),
         survival_scaling = factor(survival_scaling, c("5% increase", "10% increase", "20% increase", "Baseline - No Increase")))

facet_plot <- nat_spawn_results %>%
  ggplot(aes(year, total_spawners,
             color = survival_scaling, weight = is_baseline)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Total Natural Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 16),
        legend.position = "bottom") +
  facet_wrap(~survival_type) +
  scale_color_manual(values = color_pal, name = "")
facet_plot
ggsave("analysis/figures/facet_plot.png", facet_plot, width = 14, height = 8)

# generate lineplot function
lineplot <- function(surv_type) {
  plot <- nat_spawn_results |>
      filter(survival_type == surv_type) |>
      ggplot(aes(year, total_spawners, color = survival_scaling)) +
      geom_line() +
      theme_minimal() +
      labs(y = "Total Natural Spawners",
           x = "Simulation Year",
           title = paste("Sensitivity of", surv_type)) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = 1:20) +
      theme(text = element_text(size = 18),
            legend.position = "bottom") +
      scale_color_manual(values = color_pal, name = "")
  ggsave(paste0("analysis/figures/", "Lineplot ", surv_type, ".png"),
         plot, width = 10, height = 6)
  # ggsave(paste0("analysis/figures/", "SmallLineplot ", surv_type, ".png"),
  #        plot, width = 10, height = 3.5)
}

# Generate difference graph
difference_plot <- function(surv_type) {
  baseline <- nat_spawn_results |>
    filter(survival_type == surv_type,
           survival_scaling == "Baseline - No Increase") |>
    rename(baseline_spawners = total_spawners) |>
    select(-survival_scaling, -is_baseline)
  plot <- nat_spawn_results |>
    filter(survival_type == surv_type,
           survival_scaling != "Baseline - No Increase") |>
    left_join(baseline) |>
    mutate(difference = total_spawners - baseline_spawners) |>
    ggplot(aes(year, difference, color = survival_scaling)) +
    geom_line() +
    theme_minimal() +
    labs(y = "Difference in Number of
         Natural Spawners from Baseline",
         x = "Simulation Year",
         title = paste("Sensitivity of", surv_type)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = 1:20) +
    theme(text = element_text(size = 20),
          legend.position = "bottom") +
    scale_color_manual(values = color_pal, name = "")
  ggsave(paste0("analysis/figures/", "Difference ",
                surv_type, ".png"), plot, width = 10, height = 6)
  # ggsave(paste0("analysis/figures/", "SmallDifference ", surv_type, ".png"),
  #        plot, width = 10, height = 4)
}

# TODO see if they want CRR metrics tomorrow

# Other Plots?

# Generate Saved Graphs
surv_types = c("Ocean Entry Survival", "Prespawn Survival", "Spawning Habitat",
               "Spawning Habitat and Ocean Entry Survival", "Prespawn Survival and Ocean Entry Survival")

surv_types_2 = c("Spawning Habitat", "Rearing Habitat", "Spawning and Rearing Habitat")

# map through lineplots
purrr::map(surv_types_2, lineplot)

# map through difference plots
purrr::map(surv_types_2, difference_plot)

# sumary table of percent increase
baseline <- nat_spawn_results |>
  filter(survival_scaling == "Baseline - No Increase") |>
  rename(baseline_spawners = total_spawners) |>
  select(-survival_scaling, -is_baseline) |> glimpse()

summary_table <- nat_spawn_results |>
  select(-is_baseline) |>
  filter(survival_scaling != "Baseline - No Increase") |>
  left_join(baseline) |>
  group_by(survival_type, survival_scaling) |>
  summarise(simulation_total_spawners = sum(total_spawners, na.rm = T),
            simulation_baseline_spawners = sum(baseline_spawners, na.rm = T),
            difference = simulation_total_spawners - simulation_baseline_spawners,
            percent_diff = difference / simulation_total_spawners * 100)
View(summary_table)
