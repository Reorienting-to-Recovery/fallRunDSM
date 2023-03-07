library(tidyverse)
library(fallRunDSM)
library(plotly)
# Source helper functions
source("data-raw/helper_graph_functions.R")
# View prelim R2R results
set.seed(123119)
r2r_seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::r_to_r_baseline_params)

r2r_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
                                                seeds = r2r_seeds)
# BASIC ABUNDANCE PLOTS
# Total natural spawners plot --------------------------------------------------
plot_total_spawners(r2r_model_results, "Total Natural Spawners")

# Total Spawners (natural and hatchery )
plot_total_spawners(r2r_model_results, "Total Spawners")

# Single watershed total spawners
plot_single_watershed_natural_spawners(model_results = r2r_model_results,
                                       watershed = "Battle Creek",
                                       result_type = "Total Spawners")

# All watershed
plot_all_watersheds_spawners(model_results = r2r_model_results,
                             result_type = "Total Spawners")
plot_all_watersheds_spawners(model_results = r2r_model_results,
                             result_type = "Total Natural Spawners")
# CCR Results ------------------------------------------------------------------
# Adult to adult metric --------------------------------------------------------
# Data processing
# TODO these show an example where we just plot the mean - the adult to adult plots
# show an example where we plot each watershed. Confirm which route we want to go?
plot_juv_crr(model_results = r2r_model_results,
               result_type = "Total Spawners")

plot_juv_crr(model_results = r2r_model_results,
             result_type = "Total Natural Spawners")
## CCR adults to adults --------------------------------------------------------
# data processing
# TODO confirm we do not want total spawn and natural spawn compared against each other
plot_adult_crr(model_results = r2r_model_results,
               result_type = "Total Spawners")

plot_adult_crr(model_results = r2r_model_results,
               result_type = "Total Natural Spawners")

# Habitat percentages by watershed ---------------------------------------------
produce_habitat_ratios(model_parameters = r_to_r_baseline_params, watershed = "Yuba River")
produce_habitat_ratios(model_parameters = r_to_r_baseline_params, watershed = "Lower Sacramento River")


### OG MODEL COMPARISON ########################################################
# OG model results
set.seed(123119)
seeds <- fallRunDSM::fall_run_model(mode = "seed", ..params = fallRunDSM::params)

model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::params, seeds = seeds)

plot_total_natural_spawners(model_results, "Total Natural Spawners")
