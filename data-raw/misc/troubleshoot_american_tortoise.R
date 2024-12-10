new_params <- fallRunDSM::r_to_r_baseline_params
new_params$movement_hypo_weights <- c(1, rep(0, 7))
new_params$san_joaquin_flows <- DSMflow::flows_cfs$biop_itp_2018_2019 |>
  select(date, `San Joaquin River`) |>
  mutate(sjQcms = DSMflow::cfs_to_cms(`San Joaquin River`),
         year = year(date),
         month = month(date)) |>
  filter(year >= 1980, year <= 2000) |>
  arrange(date, ascending = TRUE) |>
  select(-date, -`San Joaquin River`) |>
  pivot_wider(names_from = year,
              values_from = sjQcms) |>
  select(-month) |>
  as.matrix()

baseline_params_new <- load_scenario(scenarios$baseline_scenarios$baseline,
                                     new_params,
                                     "fr")

baseline_test <- data.frame(
  watershed = rep("All", 4),
  action = c(1, 23, 10, 18), #16, #5, 7, 25, 16, 17, 11, 20, 26, 29),
  years =  I(list("All", c(dry_years), "All", "All")) #, "All", "All", "All", "All"))
)

baseline_w_eff_params <- load_scenario(baseline_test,
                                          new_params,
                                          "fr")

# checked DSMhabitat: baseline and eff baseline habitat only different for
# Sac tribs, not for American

# could it be updates to existing habitat?
# no: these parameters are actually lower for american in the new baseline
square_meters_to_acres(r_to_r_baseline_params$spawning_habitat)["American River",,] -
  square_meters_to_acres(baseline_params_new$spawning_habitat)["American River",,]
square_meters_to_acres(r_to_r_baseline_params$inchannel_habitat_fry)["American River",,] -
  square_meters_to_acres(baseline_params_new$inchannel_habitat_fry)["American River",,]
square_meters_to_acres(r_to_r_baseline_params$inchannel_habitat_juvenile)["American River",,] -
  square_meters_to_acres(baseline_params_new$inchannel_habitat_juvenile)["American River",,]

# what's going on here
# American habitat for r2r_eff_baseline are the same as baseline.
square_meters_to_acres(baseline_params_new$spawning_habitat)["American River",,] -
  square_meters_to_acres(baseline_w_eff_params$spawning_habitat)["American River",,]
square_meters_to_acres(baseline_params_new$inchannel_habitat_fry)["American River",,] -
  square_meters_to_acres(baseline_w_eff_params$inchannel_habitat_fry)["American River",,]
square_meters_to_acres(baseline_params_new$inchannel_habitat_juvenile)["American River",,] -
  square_meters_to_acres(baseline_w_eff_params$inchannel_habitat_juvenile)["American River",,]

# could it be hatchery?
r_to_r_baseline_params$hatchery_release[,,1] -baseline_params_new$hatchery_release[,,1]
# no change in baselines
baseline_w_eff_params$hatchery_release[,,1] - baseline_params_new$hatchery_release[,,1]


# now do the check_res ----------------------------------------------------

# baseline old vs baseline new
check_scenarios <- function(names) {
  print(names)
  correct_res <- r_to_r_baseline_params[[names]] != baseline_params_new[[names]]
  return(sum(correct_res))
}

res <- map(names(fallRunDSM::r_to_r_baseline_params), check_scenarios)
which(res > 0)

names(fallRunDSM::r_to_r_baseline_params)[which(res>0)]

# baseline new vs baseline w eff
check_scenarios <- function(names) {
  print(names)
  correct_res <- baseline_params_new[[names]] != baseline_w_eff_params[[names]]
  return(sum(correct_res))
}

res <- map(names(baseline_params_new), check_scenarios)
which(res > 0)

names(r_to_r_baseline_params)[which(res>0)]

