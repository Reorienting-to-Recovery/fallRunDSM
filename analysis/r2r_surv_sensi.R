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


sensi_model_run(sensi_type = "spawning habitat", sensi_quantity = 1.05)

sensi_types <- c("prespawn surv", "prespawn surv", "prespawn surv","prespawn surv",
                 "spawning habitat", "spawning habitat", "spawning habitat", "spawning habitat",
                 "spawning and ocean", "spawning and ocean","spawning and ocean","spawning and ocean",
                 "prespawn and ocean", "prespawn and ocean", "prespawn and ocean", "prespawn and ocean",
                 "ocean surv", "ocean surv", "ocean surv", "ocean surv")
sensi_quantity <- c(rep(c(1.05, 1.1, 1.2, 1), 5))

model_results <- purrr::map2(sensi_types, sensi_quantity, sensi_model_run) |>
  purrr::reduce(bind_rows)

#
saveRDS(model_results, "analysis/r_2_r_sensi_results.rds")
# Total natural spawners plot
model_results %>%
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year")  |>
  mutate(survival_scaling = case_when(survival_scaling == 1.05 ~ "5% increase",
                                      survival_scaling == 1.10 ~ "10% increase",
                                      survival_scaling == 1.20 ~ "20% increase",
                                      survival_scaling == 1 ~ "Baseline - No Increase"),
         survival_type = case_when(survival_type == "prespawn surv" ~ "Prespawn Survival Results",
                                   survival_type == "ocean surv" ~ "Ocean Entry Survival Results",
                                   survival_type == "spawning habitat" ~ "Spawning Habitat Results",
                                   survival_type == "spawning and ocean" ~ "Spawning Habitat and Ocean Entry Survival Results",
                                   survival_type == "prespawn and ocean" ~ "Prespawn Survival and Ocean Entry Survival Results")) |>
  group_by(year, survival_type, survival_scaling) |>
  summarize(total_spawners = sum(natural_spawners)) |>
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners, color = survival_scaling)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Total Natural Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20),
        legend.position = "bottom") +
  facet_wrap(~survival_type)


model_results %>%
  filter(survival_type == "ocean surv") |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'natural_spawners', names_to = "year")  |>
  mutate(survival_scaling = case_when(survival_scaling == 1.05 ~ "5% increase",
                                      survival_scaling == 1.10 ~ "10% increase",
                                      survival_scaling == 1.20 ~ "20% increase",
                                      survival_scaling == 1 ~ "Baseline - No Increase"),
         survival_type = case_when(survival_type == "prespawn surv" ~ "Prespawn Survival Results",
                                   survival_type == "ocean surv" ~ "Ocean Entry Survival Results",
                                   survival_type == "spawning habitat" ~ "Spawning Habitat Results",
                                   survival_type == "spawning and ocean" ~ "Spawning Habitat and Ocean Entry Survival Results",
                                   survival_type == "prespawn and ocean" ~ "Prespawn Survival and Ocean Entry Survival Results")) |>
  group_by(year, survival_type, survival_scaling) |>
  summarize(total_spawners = sum(natural_spawners)) |>
  mutate(year = as.numeric(year)) |>
  ggplot(aes(year, total_spawners, color = survival_scaling)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Total Natural Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20),
        legend.position = "bottom")
