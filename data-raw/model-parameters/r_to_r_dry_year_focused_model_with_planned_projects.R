library(tidyverse)
# remotes::install_github("Reorienting-to-Recovery/DSMflow")
# remotes::install_github("Reorienting-to-Recovery/DSMhabitat@eff_sac_hab")
# remotes::install_github("Reorienting-to-Recovery/DSMtemperature")
library(DSMhabitat)
library(DSMflow)
library(DSMtemperature)


# Description of dry year scenario
# In dry years add rice field habitat in the yolo bypass (900 acres in Feb (maybe jan too?))
# TODO
dry_years <- waterYearType::water_year_indices |>
  filter(location == "Sacramento Valley",
         WY > 1979 & WY < 2001) |>
  rename(water_year = WY) |>
  mutate(year_type = ifelse(Yr_type %in% c("Wet", "Above Normal"), "wet", "dry"),
         model_year = c(1:21)) |>
  select(water_year, year_type, model_year) |>
  glimpse()

yolo_hab <- DSMhabitat::yolo_habitat$biop_itp_2018_2019
yolo_hab[1:2, c(2, 6, 8:13, 15)] <- yolo_hab[1:2, c(2, 6, 8:13, 15)] + DSMhabitat::acres_to_square_meters(9000)
# Dry year, food subsidies (TODO, right now being applied in all years) to
# sacramento (lower and lower mid), butte, yuba, and feather, sutter, and yolo
# (food subsidies added more places because they include outside of levee habitat)
rice_field_prey_increase_prey_density <- fallRunDSM::prey_density
rice_field_prey_increase_prey_density[24] <- "max" # lower sac
rice_field_prey_increase_prey_density[21] <- "max" # lower-mid sac
rice_field_prey_increase_prey_density[16] <- "max" # sutter
rice_field_prey_increase_prey_density[22] <- "max" # yolo
rice_field_prey_increase_prey_density[6] <- "max" # butte
rice_field_prey_increase_prey_density[19] <- "max" # feather
rice_field_prey_increase_prey_density[20] <- "max" # yuba

# EFF dry years sac
up_sac_flow <- DSMflow::upper_sacramento_flows$biop_itp_2018_2019
up_sac_flow[, c(2, 6, 8:13, 15)] <- DSMflow::upper_sacramento_flows$eff_sac[, c(2, 6, 8:13, 15)]

fry <- DSMhabitat::fr_fry$r_to_r_baseline
fry[,, c(2, 6, 8:13, 15)] <- DSMhabitat::fr_fry$r_to_r_eff_baseline[,, c(2, 6, 8:13, 15)]

juv <- DSMhabitat::fr_juv$r_to_r_baseline
juv[,, c(2, 6, 8:13, 15)] <- DSMhabitat::fr_juv$r_to_r_eff_baseline[,, c(2, 6, 8:13, 15)]

fp <- DSMhabitat::fr_fp$r_to_r_baseline
fp[,, c(2, 6, 8:13, 15)] <- DSMhabitat::fr_fp$r_to_r_eff_baseline[,, c(2, 6, 8:13, 15)]

spawn <- DSMhabitat::fr_spawn$r_to_r_baseline
spawn[,, c(2, 6, 8:13, 15)] <- DSMhabitat::fr_spawn$r_to_r_eff_baseline[,, c(2, 6, 8:13, 15)]
# loads calibration data
calib_results <- read_rds("calibration/r2r-results-2023-12-11.rds")
solution <- calib_results@solution

harvest_percentage <- fallRunDSM::r2r_adult_harvest_rate - rep(.5, 31)
harvest_percentage[harvest_percentage < 0] <- 0


# initial params
r_to_r_dry_years_params_with_projects <- list(
  spawn_decay_multiplier = DSMhabitat::spawning_decay_multiplier$biop_itp_2018_2019$fr,

  # Data from DSMscenarios
  spawn_decay_rate = DSMscenario::spawn_decay_rate,
  rear_decay_rate = DSMscenario::rear_decay_rate,

  # Data from fallRunDSM cache-data (values vary by run)
  hatchery_allocation = fallRunDSM::hatchery_allocation,
  natural_adult_removal_rate = fallRunDSM::natural_adult_removal_rate,
  proportion_hatchery = fallRunDSM::proportion_hatchery,
  month_return_proportions = fallRunDSM::month_return_proportions,
  growth_rates = fallRunDSM::growth_rates_inchannel,
  growth_rates_floodplain = fallRunDSM::growth_rates_floodplain,
  mass_by_size_class = fallRunDSM::mass_by_size_class,
  cross_channel_stray_rate = fallRunDSM::cross_channel_stray_rate,
  stray_rate = fallRunDSM::stray_rate,
  diversity_group = fallRunDSM::diversity_group,
  crr_scaling = 2, # defaults to 2

  # Coefficients for adult submodules
  .adult_stray_intercept = 3,
  .adult_stray_wild = -5.5,
  .adult_stray_natal_flow = -1.99,
  .adult_stray_cross_channel_gates_closed = -0.174,
  .adult_stray_prop_bay_trans = 2.09,
  .adult_stray_prop_delta_trans = 2.89,
  .adult_en_route_migratory_temp = -0.26,
  .adult_en_route_bypass_overtopped = -0.019,
  .adult_prespawn_deg_day = -0.000669526,

  # Ocean entry success coefficient and variable
  .ocean_entry_success_length = c(-0.0897309864, -0.0709704348, -0.0208590732, 0.0732620916),
  .ocean_entry_success_months = 0.35,

  # Routing coefficients and variables
  .pulse_movement_intercept = -7.70744,
  .pulse_movement_proportion_pulse = 0.26579,
  .pulse_movement_medium = 1.66845,
  .pulse_movement_large = 0.5706,
  .pulse_movement_vlarge = -4.305,
  .pulse_movement_medium_pulse = -0.25477,
  .pulse_movement_large_pulse = -0.44778,
  .pulse_movement_very_large_pulse = 0.329,
  territory_size = c(0.0498944803729701, 0.138941944739835, 0.471083652829798, 0),

  # Spawn success variables
  spawn_success_sex_ratio = 0.5,
  spawn_success_redd_size = 9.29,
  spawn_success_fecundity = 5522,

  # Egg to fry survival coefficients
  .surv_egg_to_fry_proportion_natural = 0.533,
  .surv_egg_to_fry_scour = -0.655,

  # Juvenile rearing survival coefficients and variables
  .surv_juv_rear_avg_temp_thresh = -0.717,
  .surv_juv_rear_contact_points = -0.189,
  .surv_juv_rear_prop_diversions = -3.51,
  .surv_juv_rear_total_diversions = -0.0021,
  .surv_juv_rear_high_predation = -0.122,
  .surv_juv_rear_stranded = -1.939,
  .surv_juv_rear_medium = 1.48,
  .surv_juv_rear_large = 2.223,
  .surv_juv_rear_floodplain = 0.47,
  min_survival_rate = 0.0001,

  # Juvenile bypass survival coefficients and variables
  .surv_juv_bypass_avg_temp_thresh = -0.717,
  .surv_juv_bypass_high_predation = -0.122,
  .surv_juv_bypass_medium = 1.48,
  .surv_juv_bypass_large = 2.223,
  .surv_juv_bypass_floodplain = 0.47,

  # Juvenile delta survival coefficients and variables
  .surv_juv_delta_avg_temp_thresh = -0.717,
  .surv_juv_delta_contact_points = -0.189,
  .surv_juv_delta_total_diverted = -0.0021,
  .surv_juv_delta_high_predation = -0.122,
  .surv_juv_delta_prop_diverted = -3.51,
  .surv_juv_delta_medium = 1.48,
  .surv_juv_delta_large = 2.223,

  # San joaquin outmigration variables
  .surv_juv_outmigration_san_joaquin_medium = 1.48,
  .surv_juv_outmigration_san_joaquin_large = 2.223,

  ## Variable from load baseline data
  # DSMflow variables -----
  freeport_flows = DSMflow::freeport_flow$biop_itp_2018_2019,
  vernalis_flows = DSMflow::vernalis_flow$biop_itp_2018_2019,
  stockton_flows = DSMflow::stockton_flow$biop_itp_2018_2019,
  CVP_exports = DSMflow::cvp_exports$biop_itp_2018_2019,
  SWP_exports = DSMflow::swp_exports$biop_itp_2018_2019,
  proportion_diverted = DSMflow::proportion_diverted$biop_itp_2018_2019,
  total_diverted = DSMflow::total_diverted$biop_itp_2018_2019,
  delta_proportion_diverted = DSMflow::delta_proportion_diverted$biop_itp_2018_2019,
  delta_total_diverted = DSMflow::delta_total_diverted$biop_itp_2018_2019,
  prop_pulse_flows = DSMflow::proportion_pulse_flows$biop_itp_2018_2019,
  prop_flow_natal = DSMflow::proportion_flow_natal$biop_itp_2018_2019,
  upper_sacramento_flows = up_sac_flow,
  delta_inflow = DSMflow::delta_inflow$biop_itp_2018_2019,
  cc_gates_days_closed = DSMflow::delta_cross_channel_closed$biop_itp_2018_2019["count", ],
  cc_gates_prop_days_closed = DSMflow::delta_cross_channel_closed$biop_itp_2018_2019["proportion", ],
  proportion_flow_bypass = DSMflow::proportion_flow_bypasses$biop_itp_2018_2019,
  gates_overtopped = DSMflow::gates_overtopped$biop_itp_2018_2019,


  # DSMtemperature variables -----
  vernalis_temps = DSMtemperature::vernalis_temperature,
  prisoners_point_temps = DSMtemperature::prisoners_point_temperature,
  degree_days = DSMtemperature::degree_days$biop_itp_2018_2019,
  mean_egg_temp_effect = DSMtemperature::egg_temperature_effect$fall_run,
  avg_temp = DSMtemperature::stream_temperature$biop_itp_2018_2019,
  avg_temp_delta = DSMtemperature::delta_temperature,
  migratory_temperature_proportion_over_20 = DSMtemperature::migratory_temperature_proportion_over_20,

  # DSMhabitat variables -----
  spawning_habitat = spawn,
  inchannel_habitat_fry = fry, # vary by run
  inchannel_habitat_juvenile = juv, # vary by run
  floodplain_habitat = fp, # vary by run
  weeks_flooded = DSMhabitat::weeks_flooded$biop_itp_2018_2019,
  delta_habitat = DSMhabitat::delta_habitat$r_to_r_baseline,
  sutter_habitat = DSMhabitat::sutter_habitat$biop_itp_2018_2019,
  yolo_habitat = yolo_hab,
  tisdale_bypass_watershed = DSMhabitat::tisdale_bypass_watershed,
  yolo_bypass_watershed = DSMhabitat::yolo_bypass_watershed,
  south_delta_routed_watersheds = DSMhabitat::south_delta_routed_watersheds,
  prop_high_predation = DSMhabitat::prop_high_predation,
  contact_points = DSMhabitat::contact_points,
  delta_contact_points = DSMhabitat::delta_contact_points,
  delta_prop_high_predation = DSMhabitat::delta_prop_high_predation,
  prob_strand_early = DSMhabitat::prob_strand_early,
  prob_strand_late = DSMhabitat::prob_strand_late,
  prob_nest_scoured = DSMhabitat::prob_nest_scoured,

  prey_density = rice_field_prey_increase_prey_density,
  prey_density_delta = fallRunDSM::prey_density_delta,

  # Calibration Variables (vary by run)
  ..surv_adult_enroute_int = solution[1],
  ..surv_adult_prespawn_int = solution[2],
  ..surv_egg_to_fry_int = solution[3],
  ..surv_juv_rear_int = c(`Upper Sacramento River` = solution[5],
                          `Antelope Creek` = solution[8],
                          `Battle Creek` = solution[8],
                          `Bear Creek` = solution[8],
                          `Big Chico Creek` = solution[8],
                          `Butte Creek` = solution[6],
                          `Clear Creek` = solution[7],
                          `Cottonwood Creek` = solution[8],
                          `Cow Creek` = solution[8],
                          `Deer Creek` = solution[8],
                          `Elder Creek` = solution[8],
                          `Mill Creek` = solution[9],
                          `Paynes Creek` = solution[8],
                          `Stony Creek` = solution[8],
                          `Thomes Creek` = solution[8],
                          `Upper-mid Sacramento River` = solution[10],
                          `Sutter Bypass` = solution[4],
                          `Bear River` = solution[8],
                          `Feather River` = solution[11],
                          `Yuba River` = solution[12],
                          `Lower-mid Sacramento River` = solution[10],
                          `Yolo Bypass` = solution[4],
                          `American River` = solution[13],
                          `Lower Sacramento River` = solution[10],
                          `Calaveras River` = solution[14],
                          `Cosumnes River` = solution[14],
                          `Mokelumne River` = solution[15],
                          `Merced River` = solution[16],
                          `Stanislaus River` = solution[17],
                          `Tuolumne River` = solution[18],
                          `San Joaquin River` = solution[19]),
  ..surv_juv_rear_contact_points = solution[20],
  ..surv_juv_rear_prop_diversions = solution[21],
  ..surv_juv_rear_total_diversions = solution[22],
  ..surv_juv_bypass_int = solution[23],
  ..surv_juv_delta_int = solution[24],
  ..surv_juv_delta_contact_points = solution[25],
  ..surv_juv_delta_total_diverted = solution[26],
  ..surv_juv_outmigration_sj_int = solution[27],
  ..ocean_entry_success_int = c(
    `Upper Sacramento River` = solution[29],
    `Antelope Creek` = solution[28],
    `Battle Creek` = solution[28],
    `Bear Creek` = solution[28],
    `Big Chico Creek` = solution[28],
    `Butte Creek` = solution[30],
    `Clear Creek` = solution[28],
    `Cottonwood Creek` = solution[28],
    `Cow Creek` = solution[28],
    `Deer Creek` = solution[31],
    `Elder Creek` = solution[28],
    `Mill Creek` = solution[32],
    `Paynes Creek` = solution[28],
    `Stony Creek` = solution[28],
    `Thomes Creek` = solution[28],
    `Upper-mid Sacramento River` = solution[28],
    `Sutter Bypass` = solution[28],
    `Bear River` = solution[33],
    `Feather River` = solution[33],
    `Yuba River` = solution[34],
    `Lower-mid Sacramento River` = solution[28],
    `Yolo Bypass` = solution[28],
    `American River` = solution[35],
    `Lower Sacramento River` = solution[28],
    `Calaveras River` = solution[36],
    `Cosumnes River` = solution[36],
    `Mokelumne River` = solution[37],
    `Merced River` = solution[38],
    `Stanislaus River` = solution[39],
    `Tuolumne River` = solution[40],
    `San Joaquin River` = solution[28]),

  # R2R specific metrics
  hatchery_release = matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels)), # all terminal release
  hatchery_release_proportion_bay = fallRunDSM::hatchery_release_proportion_bay,
  fecundity_lookup = fallRunDSM::fecundity_by_age,
  adult_harvest_rate = fallRunDSM::r2r_adult_harvest_rate,
  restrict_harvest_to_hatchery_ocean = TRUE,
  restrict_harvest_to_hatchery_trib = TRUE,
  ocean_harvest_percentage = .5,
  tributary_harvest_percentage = harvest_percentage,
  no_cohort_harvest_years = c(2, 6, 8:13, 15), # no harvest of dry year cohorts
  intelligent_crr_harvest = FALSE,
  intelligent_habitat_harvest = FALSE, # kinda meets recovery, but huge drop
  terminal_hatchery_logic = TRUE,
  crr_scaling = 2, # defaults to 2


  # stray model
  flows_oct_nov = DSMflow::hatchery_oct_nov_flows$biop_itp_2018_2019,
  flows_apr_may = DSMflow::hatchery_apr_may_flows$biop_itp_2018_2019,

  # multi route
  movement_hypo_weights = rep(1/8, 8),
  ..habitat_capacity = 5,
  ..floodplain_capacity = 5
)

usethis::use_data(r_to_r_dry_years_params_with_projects, overwrite = TRUE)








