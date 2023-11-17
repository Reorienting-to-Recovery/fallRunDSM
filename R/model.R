#' @title Fall Run Chinook Model
#' @description Fall Run Chinook life cycle model used for CVPIA's Structured
#' Decision Making Process
#' @param scenario Model inputs, can be modified to test management actions
#' @param mode The mode to run model in. Can be \code{"seed"}, \code{"simulate"}, \code{"calibrate"}
#' @param seeds The default value is NULL runs the model in seeding mode,
#' returning a 31 by 25 matrix with the first four years of seeded adults. This
#' returned value can be fed into the model again as the value for the seeds argument
#' @param ..params Parameters for model and submodels. Defaults to \code{fallRunDSM::\code{\link{params}}}.
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model should be run stochastically. Defaults to \code{FALSE}.
#' @source IP-117068
#' @examples
#' fall_run_seeds <- fallRunDSM::fall_run_model(mode = "seed")
#' fallRunDSM::fall_run_model(scenario = DSMscenario::scenarios$ONE,
#'                            mode = "simulate",
#'                            seeds = fall_run_seeds)
#' @export
fall_run_model <- function(scenario = NULL, mode = c("seed", "simulate", "calibrate"),
                           seeds = NULL, ..params = fallRunDSM::r_to_r_baseline_params,
                           stochastic = FALSE){

  mode <- match.arg(mode)

  if (mode == "simulate") {
    if (is.null(scenario)) {
      # the do nothing scenario to force habitat degradation
      scenario <- DSMscenario::scenarios$NO_ACTION
    }

    habitats <- list(
      spawning_habitat = ..params$spawning_habitat,
      inchannel_habitat_fry = ..params$inchannel_habitat_fry,
      inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
      floodplain_habitat = ..params$floodplain_habitat,
      weeks_flooded = ..params$weeks_flooded
    )

    # check if the params has the new decay element, if yes use new function applying decay
    # to spawning, if not then use the old method. This is a temporary bit of code to allow
    # for quick comparison between two versions of the model.
    if ("spawn_decay_multiplier" %in% names(..params)) {
      scenario_data <- DSMscenario::load_scenario(scenario,
                                                  habitat_inputs = habitats,
                                                  species = DSMscenario::species$FALL_RUN,
                                                  spawn_decay_rate = ..params$spawn_decay_rate,
                                                  rear_decay_rate = ..params$rear_decay_rate,
                                                  spawn_decay_multiplier = ..params$spawn_decay_multiplier,
                                                  stochastic = stochastic)
    } else {
      scenario_data <- DSMscenario::load_scenario(scenario,
                                                  habitat_inputs = habitats,
                                                  species = DSMscenario::species$FALL_RUN,
                                                  spawn_decay_rate = ..params$spawn_decay_rate,
                                                  rear_decay_rate = ..params$rear_decay_rate,
                                                  stochastic = stochastic)
    }

    ..params$spawning_habitat <- scenario_data$spawning_habitat
    ..params$inchannel_habitat_fry <- scenario_data$inchannel_habitat_fry
    ..params$inchannel_habitat_juvenile <- scenario_data$inchannel_habitat_juvenile
    ..params$floodplain_habitat <- scenario_data$floodplain_habitat
    ..params$weeks_flooded <- scenario_data$weeks_flooded

  }

  if (mode == "calibrate") {
    scenario_data <- list(
      survival_adjustment = matrix(1, nrow = 31, ncol = 21,
                                   dimnames = list(DSMscenario::watershed_labels,
                                                   1980:2000)))
  }

  simulation_length <- switch(mode,
                              "seed" = 5,
                              "simulate" = 20,
                              "calibrate" = 20)

  output <- list(

    # SIT METRICS
    spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    juvenile_biomass = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    returning_adults = tibble::tibble(),

    # R2R METRICS
    adults_in_ocean = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    juveniles = data.frame(),
    juveniles_at_chipps = data.frame(),
    proportion_natural_at_spawning = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    proportion_natural_juves_in_tribs = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    phos = matrix(0, nrow = 31, ncol = 20, dimnames = list(fallRunDSM::watershed_labels, 1:20)),
    limiting_habitat = data.frame()
  )


  if (mode == 'calibrate') {
    calculated_adults <- matrix(0, nrow = 31, ncol = 30)
  }

  adults <- switch (mode,
                    "seed" = fallRunDSM::adult_seeds,
                    "simulate" = seeds,
                    "calibrate" = seeds,
  )



  for (year in 1:simulation_length) {
    adults_in_ocean <- numeric(31)
    # initialize 31 x 4 matrices for natal fish, migrants, and ocean fish
    lower_mid_sac_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:20], fallRunDSM::size_class_labels))
    lower_sac_fish <- matrix(0, nrow = 27, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:27], fallRunDSM::size_class_labels))
    upper_mid_sac_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:15], fallRunDSM::size_class_labels))
    sutter_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:15], fallRunDSM::size_class_labels))
    yolo_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:20], fallRunDSM::size_class_labels))
    san_joaquin_fish <- matrix(0, nrow = 3, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[28:30], fallRunDSM::size_class_labels))
    north_delta_fish <- matrix(0, nrow = 23, ncol = 4, dimnames = list(fallRunDSM::watershed_labels[1:23], fallRunDSM::size_class_labels))
    south_delta_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))
    juveniles_at_chipps <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))

    avg_ocean_transition_month <- ocean_transition_month(stochastic = stochastic) # 2

    # R2R hatchery logic update ------------------------------------------------
    hatch_adults <- if (year %in% c(1, 2)) {
      round(mean(c(83097.01,532203.1)) * ..params$hatchery_allocation)
    } else {
      hatch_adults <- output$returning_adults |>
        filter(return_sim_year == year, origin == "hatchery") |>
        group_by(watershed) |>
        summarise(hatchery_total = sum(return_total, na.rm = TRUE)) |>
        deframe()
      unname(hatch_adults[order(match(names(hatch_adults), watershed_labels))])
    }
    # end updated logic --------------------------------------------------------

    # returning adults are here: round(adults)
    spawners <- get_spawning_adults(year, round(adults), hatch_adults, mode = mode,
                                    month_return_proportions = ..params$month_return_proportions,
                                    prop_flow_natal = ..params$prop_flow_natal,
                                    south_delta_routed_watersheds = ..params$south_delta_routed_watersheds,
                                    cc_gates_days_closed = ..params$cc_gates_days_closed,
                                    gates_overtopped = ..params$gates_overtopped,
                                    tisdale_bypass_watershed = ..params$tisdale_bypass_watershed,
                                    yolo_bypass_watershed = ..params$yolo_bypass_watershed,
                                    migratory_temperature_proportion_over_20 = ..params$migratory_temperature_proportion_over_20,
                                    natural_adult_removal_rate = ..params$natural_adult_removal_rate,
                                    cross_channel_stray_rate = ..params$cross_channel_stray_rate,
                                    stray_rate = ..params$stray_rate,
                                    total_releases = ..params$hatchery_release,
                                    release_month = 1,
                                    flows_oct_nov = ..params$flows_oct_nov,
                                    flows_apr_may = ..params$flows_apr_may,
                                    monthly_mean_pdo = fallRunDSM::monthly_mean_pdo,
                                    ..surv_adult_enroute_int = ..params$..surv_adult_enroute_int,
                                    .adult_stray_intercept = ..params$.adult_stray_intercept,
                                    .adult_stray_wild = ..params$.adult_stray_wild,
                                    .adult_stray_natal_flow = ..params$.adult_stray_natal_flow,
                                    .adult_stray_cross_channel_gates_closed = ..params$.adult_stray_cross_channel_gates_closed,
                                    .adult_stray_prop_bay_trans = ..params$.adult_stray_prop_bay_trans,
                                    .adult_stray_prop_delta_trans = ..params$.adult_stray_prop_delta_trans,
                                    .adult_en_route_migratory_temp = ..params$.adult_en_route_migratory_temp,
                                    .adult_en_route_bypass_overtopped = ..params$.adult_en_route_bypass_overtopped,
                                    .adult_en_route_adult_harvest_rate = ..params$.adult_en_route_adult_harvest_rate,
                                    stochastic = stochastic)

    init_adults <- spawners$init_adults
    output$spawners[ , year] <- init_adults

    # # For use in the r2r metrics ---------------------------------------------
    # TODO add conditional here for if hatch release == 0
    phos <- 1 - spawners$proportion_natural
    if (year > 3){
      phos_diff_two_years <- phos - output$phos[, (year - 2)]
      phos_diff_last_year <- phos - output$phos[, (year - 1)]
      if (any(phos_diff_two_years < 0 & phos_diff_last_year < 0)) {
        perc_diff <- (phos - output$phos[, (year - 2)]) /  output$phos[, (year - 2)]
        renaturing_tribs <- which(phos_diff_two_years < 0 & phos_diff_last_year < 0)
        proportion_renaturing <- ifelse(names(phos_diff_two_years) %in% names(renaturing_tribs), abs(phos_diff_two_years), 0)

        total_renaturing_in_year <- spawners$init_adults * (1 - spawners$proportion_natural) * proportion_renaturing
        total_natural_with_renaturing <- total_renaturing_in_year + spawners$init_adults * spawners$proportion_natural
        natural_proportion_with_renat <-  total_natural_with_renaturing / spawners$init_adults
        natural_proportion_with_renat <- ifelse(is.nan(natural_proportion_with_renat), 0, natural_proportion_with_renat)

      } else {
        natural_proportion_with_renat <-  spawners$proportion_natural
      }
    } else {
      natural_proportion_with_renat <-  spawners$proportion_natural

    }

    output$proportion_natural_at_spawning[ , year] <- natural_proportion_with_renat
    output$phos[ , year] <- 1 - natural_proportion_with_renat
    # end R2R metric logic -----------------------------------------------------

    egg_to_fry_surv <- surv_egg_to_fry(
      proportion_natural = natural_proportion_with_renat, # update to new prop nat (renaturing logic applied)
      scour = ..params$prob_nest_scoured,
      temperature_effect = ..params$mean_egg_temp_effect,
      .proportion_natural = ..params$.surv_egg_to_fry_proportion_natural,
      .scour = ..params$.surv_egg_to_fry_scour,
      ..surv_egg_to_fry_int = ..params$..surv_egg_to_fry_int
    )

    min_spawn_habitat <- apply(..params$spawning_habitat[ , 10:12, year], 1, min)

    accumulated_degree_days <- cbind(oct = rowSums(..params$degree_days[ , 10:12, year]),
                                     nov = rowSums(..params$degree_days[ , 11:12, year]),
                                     dec = ..params$degree_days[ , 12, year])

    average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, ..params$month_return_proportions)

    prespawn_survival <- surv_adult_prespawn(average_degree_days,
                                             ..surv_adult_prespawn_int = ..params$..surv_adult_prespawn_int,
                                             .deg_day = ..params$.adult_prespawn_deg_day)

    # # For use in the r2r metrics ---------------------------------------------
    # Adding in limiting spawning habitat here
    capacity <- min_spawn_habitat / fallRunDSM::params$spawn_success_redd_size
    at_cap <- ifelse(capacity - init_adults <= 0, TRUE, FALSE)
    lim_hab <- tibble(watershed = names(capacity),
                      capacity = capacity,
                      init_adults = init_adults,
                      habitat_limited = at_cap,
                      month = NA) |>
      dplyr::mutate(habitat_type = "spawning")
    output$limiting_habitat   <- dplyr::bind_rows(output$limiting_habitat, lim_hab)
    # end R2R metric -----------------------------------------------------------
    # R2R logic to add fish size as an input -----------------------------------
    if (year %in% c(1:6)) {
      hatch_age_dist <- tibble(watershed = fallRunDSM::watershed_labels,
                               prop_2 = rep(.3, 31),
                               prop_3 = rep(.6, 31),
                               prop_4 = rep(.1, 31))
      natural_age_dist <- tibble(watershed = fallRunDSM::watershed_labels,
                               prop_2 = rep(.22, 31),
                               prop_3 = rep(.47, 31),
                               prop_4 = rep(.26, 31),
                               prop_5 = rep(.05, 31))
    } else {
      # TODO would be good to functionalize this age_dist - takes in origin, output$returning_adults, year
      # TODO move to base r logic / remove dependencides on tidyverse
      hatch_age_dist <- output$returning_adults |>
        dplyr::filter(return_sim_year == year, origin == "hatchery") |>
        dplyr::mutate(age = return_sim_year - sim_year,
                      return_total = ifelse(is.nan(return_total), 0, return_total)) |>
        dplyr::group_by(watershed, age) |>
        dplyr::summarise(total = sum(return_total, na.rm = TRUE)) |>
        tidyr::pivot_wider(names_from = age, values_from = total) |>
        dplyr::mutate(total = `2` + `3` + `4`,
               prop_2 = ifelse(total == 0, .3, `2`/total), #need to allow for straying fish even if total pop was initially 0
               prop_3 = ifelse(total == 0, .6, `3`/total),
               prop_4 = ifelse(total == 0, .1, `4`/total)) |>
        dplyr::select(-c(`2`, `3`, `4`, total))

      # find natural age distribution
      natural_age_dist <- output$returning_adults |>
        dplyr::filter(return_sim_year == year, origin == "natural") |>
        dplyr::mutate(age = return_sim_year - sim_year,
                      return_total = ifelse(is.nan(return_total), 0, return_total)) |>
        dplyr::group_by(watershed, age) |>
        dplyr::summarise(total = sum(return_total, na.rm = TRUE)) |>
        tidyr::pivot_wider(names_from = age, values_from = total) |>
        dplyr::mutate(total = `2` + `3` + `4` + `5`,
               prop_2 = ifelse(total == 0, .22, `2`/total), #need to allow for straying fish even if total pop was initially 0
               prop_3 = ifelse(total == 0, .47, `3`/total),
               prop_4 = ifelse(total == 0, .26, `4`/total),
               prop_5 = ifelse(total == 0, .05, `5`/total)) |>
        dplyr::select(-c(`2`, `3`, `4`, `5`, total))
    }
    # end R2R logic ------------------------------------------------------------
    juveniles <- spawn_success(escapement = init_adults,
                               proportion_natural = natural_proportion_with_renat, # R2R ADDS NEW PARAM
                               hatchery_age_distribution = hatch_age_dist, # R2R ADDS NEW PARAM
                               natural_age_distribution = natural_age_dist, # R2R ADDS NEW PARAM
                               fecundity_lookup = ..params$fecundity_lookup, # R2R ADDS NEW PARAM
                               adult_prespawn_survival = prespawn_survival,
                               egg_to_fry_survival = egg_to_fry_surv,
                               prob_scour = ..params$prob_nest_scoured,
                               spawn_habitat = min_spawn_habitat,
                               sex_ratio = ..params$spawn_success_sex_ratio,
                               redd_size = ..params$spawn_success_redd_size,
                               fecundity = ..params$spawn_success_fecundity,
                               stochastic = stochastic)

    # R2R hatchery logic -------------------------------------------------------
    # Currently adds only on major hatchery rivers (American, Battle, Feather, Merced, Moke)
    # Add all as large fish
    total_juves_pre_hatchery <- rowSums(juveniles)
    natural_juveniles <- total_juves_pre_hatchery  * natural_proportion_with_renat
    total_juves_pre_hatchery <- rowSums(juveniles)
    # TODO add ability to vary release per year
    # TODO(stray) capture parameters for calculating straying
    juveniles <- juveniles + ..params$hatchery_release
    # stray_rates_in_river_releases <- hatchery_adult_stray(hatchery = )

    stopifnot(nrow(juveniles) == 31)

    # Create new prop natural including hatch releases that we can use to apply to adult returns
    proportion_natural_juves_in_tribs <- natural_juveniles / rowSums(juveniles)
    output$proportion_natural_juves_in_tribs[ , year] <- proportion_natural_juves_in_tribs

    # # For use in the r2r metrics ---------------------------------------------
    d <- data.frame(juveniles)
    colnames(d) <- c("s", "m", "l", "vl")
    d$watershed <- fallRunDSM::watershed_labels
    d <- d |> tidyr::pivot_longer(names_to = "size", values_to = "juveniles", -watershed)
    d$year <- year
    output$juveniles <- dplyr::bind_rows(output$juveniles, d)
    # end R2R metric -----------------------------------------------------------

    # TODO Some temperatures are over the 28C limit, for now I am going to
    # just make these be 28. Both of these cases in the 20 years of data
    # are barely over 28 so I think for now this is justified. With that
    # said we need to make this change in the cached data itself and make
    # a note of it.

    growth_temps <- ..params$avg_temp
    growth_temps[which(growth_temps > 28)] <- 28

    for (month in 1:8) {

      growth_rates_ic <- get_growth_rates(growth_temps[,month, year],
                                          prey_density = ..params$prey_density)

      growth_rates_fp <- get_growth_rates(growth_temps[,month, year],
                                          prey_density = ..params$prey_density,
                                          floodplain = TRUE)

      growth_rates_delta <- get_growth_rates(..params$avg_temp_delta[month, year,],
                                             prey_density = ..params$prey_density_delta)

      habitat <- get_habitat(year, month,
                             inchannel_habitat_fry = ..params$inchannel_habitat_fry,
                             inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
                             floodplain_habitat = ..params$floodplain_habitat,
                             sutter_habitat = ..params$sutter_habitat,
                             yolo_habitat = ..params$yolo_habitat,
                             delta_habitat = ..params$delta_habitat)

      rearing_survival <- get_rearing_survival(year, month,
                                               survival_adjustment = scenario_data$survival_adjustment,
                                               mode = mode,
                                               avg_temp = ..params$avg_temp,
                                               avg_temp_delta = ..params$avg_temp_delta,
                                               prob_strand_early = ..params$prob_strand_early,
                                               prob_strand_late = ..params$prob_strand_late,
                                               proportion_diverted = ..params$proportion_diverted,
                                               total_diverted = ..params$total_diverted,
                                               delta_proportion_diverted = ..params$delta_proportion_diverted,
                                               delta_total_diverted = ..params$delta_total_diverted,
                                               weeks_flooded = ..params$weeks_flooded,
                                               prop_high_predation = ..params$prop_high_predation,
                                               contact_points = ..params$contact_points,
                                               delta_contact_points = ..params$delta_contact_points,
                                               delta_prop_high_predation = ..params$delta_prop_high_predation,
                                               ..surv_juv_rear_int= ..params$..surv_juv_rear_int,
                                               .surv_juv_rear_contact_points = ..params$.surv_juv_rear_contact_points,
                                               ..surv_juv_rear_contact_points = ..params$..surv_juv_rear_contact_points,
                                               .surv_juv_rear_prop_diversions = ..params$.surv_juv_rear_prop_diversions,
                                               ..surv_juv_rear_prop_diversions = ..params$..surv_juv_rear_prop_diversions,
                                               .surv_juv_rear_total_diversions = ..params$.surv_juv_rear_total_diversions,
                                               ..surv_juv_rear_total_diversions = ..params$..surv_juv_rear_total_diversions,
                                               ..surv_juv_bypass_int = ..params$..surv_juv_bypass_int,
                                               ..surv_juv_delta_int = ..params$..surv_juv_delta_int,
                                               .surv_juv_delta_contact_points = ..params$.surv_juv_delta_contact_points,
                                               ..surv_juv_delta_contact_points = ..params$..surv_juv_delta_contact_points,
                                               .surv_juv_delta_total_diverted = ..params$.surv_juv_delta_total_diverted,
                                               ..surv_juv_delta_total_diverted = ..params$..surv_juv_delta_total_diverted,
                                               .surv_juv_rear_avg_temp_thresh = ..params$.surv_juv_rear_avg_temp_thresh,
                                               .surv_juv_rear_high_predation = ..params$.surv_juv_rear_high_predation,
                                               .surv_juv_rear_stranded = ..params$.surv_juv_rear_stranded,
                                               .surv_juv_rear_medium = ..params$.surv_juv_rear_medium,
                                               .surv_juv_rear_large = ..params$.surv_juv_rear_large,
                                               .surv_juv_rear_floodplain = ..params$.surv_juv_rear_floodplain,
                                               .surv_juv_bypass_avg_temp_thresh = ..params$.surv_juv_bypass_avg_temp_thresh,
                                               .surv_juv_bypass_high_predation = ..params$.surv_juv_bypass_high_predation,
                                               .surv_juv_bypass_medium = ..params$.surv_juv_bypass_medium,
                                               .surv_juv_bypass_large = ..params$.surv_juv_bypass_large,
                                               .surv_juv_bypass_floodplain = ..params$.surv_juv_bypass_floodplain,
                                               .surv_juv_delta_avg_temp_thresh = ..params$.surv_juv_delta_avg_temp_thresh,
                                               .surv_juv_delta_high_predation = ..params$.surv_juv_delta_high_predation,
                                               .surv_juv_delta_prop_diverted = ..params$.surv_juv_delta_prop_diverted,
                                               .surv_juv_delta_medium = ..params$.surv_juv_delta_medium,
                                               .surv_juv_delta_large = ..params$.surv_juv_delta_large,
                                               min_survival_rate = ..params$min_survival_rate,
                                               stochastic = stochastic)
      # rearing_survival$inchannel[18, ] <- rearing_survival$inchannel[19, ]
      # rearing_survival$floodplain[18, ] <- rearing_survival$floodplain[19, ]

      migratory_survival <- get_migratory_survival(year, month,
                                                   cc_gates_prop_days_closed = ..params$cc_gates_prop_days_closed,
                                                   freeport_flows = ..params$freeport_flows,
                                                   vernalis_flows = ..params$vernalis_flows,
                                                   stockton_flows = ..params$stockton_flows,
                                                   vernalis_temps = ..params$vernalis_temps,
                                                   prisoners_point_temps = ..params$prisoners_point_temps,
                                                   CVP_exports = ..params$CVP_exports,
                                                   SWP_exports = ..params$SWP_exports,
                                                   upper_sacramento_flows = ..params$upper_sacramento_flows,
                                                   delta_inflow = ..params$delta_inflow,
                                                   avg_temp_delta = ..params$avg_temp_delta,
                                                   avg_temp = ..params$avg_temp,
                                                   delta_proportion_diverted = ..params$delta_proportion_diverted,
                                                   ..surv_juv_outmigration_sj_int = ..params$..surv_juv_outmigration_sj_int,
                                                   .surv_juv_outmigration_san_joaquin_medium = ..params$.surv_juv_outmigration_san_joaquin_medium,
                                                   .surv_juv_outmigration_san_joaquin_large = ..params$.surv_juv_outmigration_san_joaquin_large,
                                                   min_survival_rate = ..params$min_survival_rate,
                                                   stochastic = stochastic)

      migrants <- matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, fallRunDSM::size_class_labels))

      if (month == 8) {

        # all remaining fish outmigrate
        migrants <- juveniles

        #TODO find where the names are lost and make sure they persist all way to this
        # part of the code
        colnames(migrants) <- c("s", "m", "l", "vl")

        sutter_fish <- migrate(sutter_fish, migratory_survival$sutter, stochastic = stochastic)
        upper_mid_sac_fish <- migrate(upper_mid_sac_fish + migrants[1:15, ], migratory_survival$uppermid_sac, stochastic = stochastic)
        migrants[1:15, ] <- upper_mid_sac_fish + sutter_fish

        lower_mid_sac_fish <- migrate(lower_mid_sac_fish + migrants[1:20, ], migratory_survival$lowermid_sac, stochastic = stochastic)
        yolo_fish <- migrate(yolo_fish, migratory_survival$yolo, stochastic = stochastic)
        migrants[1:20, ] <- lower_mid_sac_fish + yolo_fish

        lower_sac_fish <- migrate(lower_sac_fish + migrants[1:27, ], migratory_survival$lower_sac, stochastic = stochastic)

        san_joaquin_fish <- migrate(migrants[28:30, ] + san_joaquin_fish, migratory_survival$san_joaquin, stochastic = stochastic)
        migrants[28:30, ] <- san_joaquin_fish

        delta_fish <- route_and_rear_deltas(year = year, month = month,
                                            migrants = round(migrants),
                                            north_delta_fish = north_delta_fish,
                                            south_delta_fish = south_delta_fish,
                                            north_delta_habitat = habitat$north_delta,
                                            south_delta_habitat = habitat$south_delta,
                                            freeport_flows = ..params$freeport_flows,
                                            cc_gates_days_closed = ..params$cc_gates_days_closed,
                                            rearing_survival_delta = rearing_survival$delta,
                                            migratory_survival_delta = migratory_survival$delta,
                                            migratory_survival_bay_delta = migratory_survival$bay_delta,
                                            juveniles_at_chipps = juveniles_at_chipps,
                                            growth_rates = growth_rates_delta,
                                            territory_size = ..params$territory_size,
                                            stochastic = stochastic)


        migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate

        juveniles_at_chipps <- delta_fish$juveniles_at_chipps
        migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate
      } else {
        # if month < 8
        # route northern natal fish stay and rear or migrate downstream ------
        upper_sac_trib_fish <-  route(year = year,
                                      month = month,
                                      juveniles = juveniles[1:15, ],
                                      inchannel_habitat = habitat$inchannel[1:15],
                                      floodplain_habitat = habitat$floodplain[1:15],
                                      prop_pulse_flows = ..params$prop_pulse_flows[1:15, ],
                                      .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                      .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                      .pulse_movement_medium = ..params$.pulse_movement_medium,
                                      .pulse_movement_large = ..params$.pulse_movement_large,
                                      .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                      .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                      .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                      .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                      territory_size = ..params$territory_size,
                                      stochastic = stochastic)

        upper_sac_trib_rear <- rear(juveniles = upper_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[1:15, ],
                                    growth = growth_rates_ic[,,1:15],
                                    floodplain_juveniles = upper_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[1:15, ],
                                    floodplain_growth = growth_rates_fp[,,1:15],
                                    weeks_flooded = ..params$weeks_flooded[1:15, month, year],
                                    stochastic = stochastic)

        juveniles[1:15, ] <- upper_sac_trib_rear$inchannel + upper_sac_trib_rear$floodplain

        # route migrant fish into Upper-mid Sac Region (fish from watersheds 1:15)
        # regional fish stay and rear
        # or migrate further downstream or in sutter bypass

        upper_mid_sac_fish <- route_regional(month = month,
                                             year = year,
                                             migrants = upper_mid_sac_fish + upper_sac_trib_fish$migrants,
                                             inchannel_habitat = habitat$inchannel[16],
                                             floodplain_habitat = habitat$floodplain[16],
                                             prop_pulse_flows = ..params$prop_pulse_flows[16, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$uppermid_sac,
                                             proportion_flow_bypass = ..params$proportion_flow_bypass,
                                             detour = 'sutter',
                                             territory_size = ..params$territory_size,
                                             stochastic = stochastic)

        sutter_fish <- route_bypass(bypass_fish = sutter_fish + upper_mid_sac_fish$detoured,
                                    bypass_habitat = habitat$sutter,
                                    migration_survival_rate = migratory_survival$sutter,
                                    territory_size = ..params$territory_size,
                                    stochastic = stochastic)

        migrants[1:15, ] <- upper_mid_sac_fish$migrants + sutter_fish$migrants

        upper_mid_sac_fish <- rear(juveniles = upper_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[16, ],
                                   growth = growth_rates_ic[,,16],
                                   floodplain_juveniles = upper_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[16, ],
                                   floodplain_growth = growth_rates_fp[,,16],
                                   weeks_flooded = rep(..params$weeks_flooded[16, month, year], nrow(upper_mid_sac_fish$inchannel)),
                                   stochastic = stochastic)

        upper_mid_sac_fish <- upper_mid_sac_fish$inchannel + upper_mid_sac_fish$floodplain

        sutter_fish <- rear(juveniles = sutter_fish$inchannel,
                            survival_rate = matrix(rep(rearing_survival$sutter, nrow(sutter_fish$inchannel)), ncol = 4, byrow = TRUE),
                            growth = growth_rates_ic[,,17],
                            stochastic = stochastic)



        # route migrant fish into Lower-mid Sac Region (fish from watersheds 18:20, and migrants from Upper-mid Sac Region)
        # regional fish stay and rear
        # or migrate further downstream  or in yolo bypass
        lower_mid_sac_trib_fish <- route(year = year,
                                         month = month,
                                         juveniles = juveniles[18:20, ],
                                         inchannel_habitat = habitat$inchannel[18:20],
                                         floodplain_habitat = habitat$floodplain[18:20],
                                         prop_pulse_flows =  ..params$prop_pulse_flows[18:20, ],
                                         .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                         .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                         .pulse_movement_medium = ..params$.pulse_movement_medium,
                                         .pulse_movement_large = ..params$.pulse_movement_large,
                                         .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                         .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                         .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                         .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                         territory_size = ..params$territory_size,
                                         stochastic = stochastic)

        lower_mid_sac_trib_rear <- rear(juveniles = lower_mid_sac_trib_fish$inchannel,
                                        survival_rate = rearing_survival$inchannel[18:20, ],
                                        growth = growth_rates_ic[,,18:20],
                                        floodplain_juveniles = lower_mid_sac_trib_fish$floodplain,
                                        floodplain_survival_rate = rearing_survival$floodplain[18:20, ],
                                        floodplain_growth = growth_rates_fp[,,18:20],
                                        weeks_flooded = ..params$weeks_flooded[18:20, month, year],
                                        stochastic = stochastic)

        juveniles[18:20, ] <- lower_mid_sac_trib_rear$inchannel + lower_mid_sac_trib_rear$floodplain
        migrants[18:20, ] <- lower_mid_sac_trib_fish$migrants

        lower_mid_sac_fish <- route_regional(month = month,
                                             year = year,
                                             migrants = lower_mid_sac_fish + migrants[1:20, ],
                                             inchannel_habitat = habitat$inchannel[21],
                                             floodplain_habitat = habitat$floodplain[21],
                                             prop_pulse_flows = ..params$prop_pulse_flows[21, , drop = FALSE],
                                             migration_survival_rate = migratory_survival$lowermid_sac,
                                             proportion_flow_bypass = ..params$proportion_flow_bypass,
                                             detour = 'yolo',
                                             territory_size = ..params$territory_size,
                                             stochastic = stochastic)

        yolo_fish <- route_bypass(bypass_fish = yolo_fish + lower_mid_sac_fish$detoured,
                                  bypass_habitat = habitat$yolo,
                                  migration_survival_rate = migratory_survival$yolo,
                                  territory_size = ..params$territory_size,
                                  stochastic = stochastic)

        migrants[1:20, ] <- lower_mid_sac_fish$migrants + yolo_fish$migrants

        lower_mid_sac_fish <- rear(juveniles = lower_mid_sac_fish$inchannel,
                                   survival_rate = rearing_survival$inchannel[21, ],
                                   growth = growth_rates_ic[,,21],
                                   floodplain_juveniles = lower_mid_sac_fish$floodplain,
                                   floodplain_survival_rate = rearing_survival$floodplain[21, ],
                                   floodplain_growth = growth_rates_fp[,,21],
                                   weeks_flooded = rep(..params$weeks_flooded[21, month, year], nrow(lower_mid_sac_fish$inchannel)),
                                   stochastic = stochastic)

        lower_mid_sac_fish <- lower_mid_sac_fish$inchannel + lower_mid_sac_fish$floodplain

        yolo_fish <- rear(juveniles = yolo_fish$inchannel,
                          survival_rate = matrix(rep(rearing_survival$yolo, nrow(yolo_fish$inchannel)), ncol = 4, byrow = TRUE),
                          growth = growth_rates_ic[,,22],
                          stochastic = stochastic)


        # route migrant fish into Lower Sac Region (fish from watershed 23, and migrants from Lower-mid Sac Region)
        # regional fish stay and rear
        # or migrate north delta
        lower_sac_trib_fish <- route(year = year,
                                     month = month,
                                     juveniles = juveniles[23, , drop = FALSE],
                                     inchannel_habitat = habitat$inchannel[23],
                                     floodplain_habitat = habitat$floodplain[23],
                                     prop_pulse_flows =  ..params$prop_pulse_flows[23, , drop = FALSE],
                                     .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                     .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                     .pulse_movement_medium = ..params$.pulse_movement_medium,
                                     .pulse_movement_large = ..params$.pulse_movement_large,
                                     .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                     .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                     .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                     .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                     territory_size = ..params$territory_size,
                                     stochastic = stochastic)

        lower_sac_trib_rear <- rear(juveniles = lower_sac_trib_fish$inchannel,
                                    survival_rate = rearing_survival$inchannel[23, , drop = FALSE],
                                    growth = growth_rates_ic[,,23],
                                    floodplain_juveniles = lower_sac_trib_fish$floodplain,
                                    floodplain_survival_rate = rearing_survival$floodplain[23, , drop = FALSE],
                                    floodplain_growth = growth_rates_fp[,,23],
                                    weeks_flooded = ..params$weeks_flooded[23, month, year],
                                    stochastic = stochastic)

        juveniles[23, ] <- lower_sac_trib_rear$inchannel + lower_sac_trib_rear$floodplain

        migrants[23, ] <- lower_sac_trib_fish$migrants

        lower_sac_fish <- route_regional(month = month,
                                         year = year,
                                         migrants = lower_sac_fish + migrants[1:27, ],
                                         inchannel_habitat = habitat$inchannel[24],
                                         floodplain_habitat = habitat$floodplain[24],
                                         prop_pulse_flows = ..params$prop_pulse_flows[24, , drop = FALSE],
                                         migration_survival_rate = migratory_survival$lower_sac,
                                         territory_size = ..params$territory_size,
                                         stochastic = stochastic)

        migrants[1:27, ] <- lower_sac_fish$migrants

        lower_sac_fish <- rear(juveniles = lower_sac_fish$inchannel,
                               survival_rate = rearing_survival$inchannel[24, ],
                               growth = growth_rates_ic[,,24],
                               floodplain_juveniles = lower_sac_fish$floodplain,
                               floodplain_survival_rate = rearing_survival$floodplain[24, ],
                               floodplain_growth = growth_rates_fp[,,24],
                               weeks_flooded = rep(..params$weeks_flooded[24, month, year], nrow(lower_sac_fish$inchannel)),
                               stochastic = stochastic)

        lower_sac_fish <- lower_sac_fish$inchannel + lower_sac_fish$floodplain

        # route southern natal fish stay and rear or migrate downstream ------

        # route migrant fish into South Delta Region (fish from watersheds 25:27)
        # regional fish stay and rear
        # or migrate to south delta
        south_delta_trib_fish <- route(year = year,
                                       month = month,
                                       juveniles = juveniles[25:27, ],
                                       inchannel_habitat = habitat$inchannel[25:27],
                                       floodplain_habitat = habitat$floodplain[25:27],
                                       prop_pulse_flows =  ..params$prop_pulse_flows[25:27, ],
                                       .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                       .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                       .pulse_movement_medium = ..params$.pulse_movement_medium,
                                       .pulse_movement_large = ..params$.pulse_movement_large,
                                       .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                       .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                       .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                       .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                       territory_size = ..params$territory_size,
                                       stochastic = stochastic)

        south_delta_trib_rear <- rear(juveniles = south_delta_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[25:27, ],
                                      growth = growth_rates_ic[,,25:27],
                                      floodplain_juveniles = south_delta_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[25:27, ],
                                      floodplain_growth = growth_rates_fp[,,25:27],
                                      weeks_flooded = ..params$weeks_flooded[25:27, month, year],
                                      stochastic = stochastic)

        juveniles[25:27, ] <- south_delta_trib_rear$inchannel + south_delta_trib_rear$floodplain

        migrants[25:27, ] <- south_delta_trib_fish$migrants

        # route migrant fish into San Joquin River (fish from watersheds 28:30)
        # regional fish stay and rear
        # or migrate to south delta

        san_joaquin_trib_fish <- route(year = year,
                                       month = month,
                                       juveniles = juveniles[28:30, ],
                                       inchannel_habitat = habitat$inchannel[28:30],
                                       floodplain_habitat = habitat$floodplain[28:30],
                                       prop_pulse_flows =  ..params$prop_pulse_flows[28:30, ],
                                       .pulse_movement_intercept = ..params$.pulse_movement_intercept,
                                       .pulse_movement_proportion_pulse = ..params$.pulse_movement_proportion_pulse,
                                       .pulse_movement_medium = ..params$.pulse_movement_medium,
                                       .pulse_movement_large = ..params$.pulse_movement_large,
                                       .pulse_movement_vlarge = ..params$.pulse_movement_vlarge,
                                       .pulse_movement_medium_pulse = ..params$.pulse_movement_medium_pulse,
                                       .pulse_movement_large_pulse = ..params$.pulse_movement_large_pulse,
                                       .pulse_movement_very_large_pulse = ..params$.pulse_movement_very_large_pulse,
                                       territory_size = ..params$territory_size,
                                       stochastic = stochastic)

        san_joaquin_trib_rear <- rear(juveniles = san_joaquin_trib_fish$inchannel,
                                      survival_rate = rearing_survival$inchannel[28:30, ],
                                      growth = growth_rates_ic[,,28:30],
                                      floodplain_juveniles = san_joaquin_trib_fish$floodplain,
                                      floodplain_survival_rate = rearing_survival$floodplain[28:30, ],
                                      floodplain_growth = growth_rates_fp[,,28:30],
                                      weeks_flooded = ..params$weeks_flooded[28:30, month, year],
                                      stochastic = stochastic)

        juveniles[28:30, ] <- san_joaquin_trib_rear$inchannel + san_joaquin_trib_rear$floodplain

        san_joaquin_fish <- route_regional(month = month,
                                           year = year,
                                           migrants = san_joaquin_fish + san_joaquin_trib_fish$migrants,
                                           inchannel_habitat = habitat$inchannel[31],
                                           floodplain_habitat = habitat$floodplain[31],
                                           prop_pulse_flows = ..params$prop_pulse_flows[31, , drop = FALSE],
                                           migration_survival_rate = migratory_survival$san_joaquin,
                                           territory_size = ..params$territory_size,
                                           stochastic = stochastic)

        migrants[28:30, ] <- san_joaquin_fish$migrants

        san_joaquin_fish <- rear(juveniles = san_joaquin_fish$inchannel,
                                 survival_rate = rearing_survival$inchannel[31, ],
                                 growth = growth_rates_ic[,,31],
                                 floodplain_juveniles = san_joaquin_fish$floodplain,
                                 floodplain_survival_rate = rearing_survival$floodplain[31, ],
                                 floodplain_growth = growth_rates_fp[,,31],
                                 weeks_flooded = rep(..params$weeks_flooded[31, month, year], nrow(san_joaquin_fish$inchannel)),
                                 stochastic = stochastic)

        san_joaquin_fish <- san_joaquin_fish$inchannel + san_joaquin_fish$floodplain

        delta_fish <- route_and_rear_deltas(year = year, month = month,
                                            migrants = round(migrants),
                                            north_delta_fish = north_delta_fish,
                                            south_delta_fish = south_delta_fish,
                                            north_delta_habitat = habitat$north_delta,
                                            south_delta_habitat = habitat$south_delta,
                                            freeport_flows = ..params$freeport_flows,
                                            cc_gates_days_closed = ..params$cc_gates_days_closed,
                                            rearing_survival_delta = rearing_survival$delta,
                                            migratory_survival_delta = migratory_survival$delta,
                                            migratory_survival_bay_delta = migratory_survival$bay_delta,
                                            juveniles_at_chipps = juveniles_at_chipps,
                                            growth_rates = growth_rates_delta,
                                            territory_size = ..params$territory_size,
                                            stochastic = stochastic)


        migrants_at_golden_gate <- delta_fish$migrants_at_golden_gate


        north_delta_fish <- delta_fish$north_delta_fish
        south_delta_fish <- delta_fish$south_delta_fish
        juveniles_at_chipps <- delta_fish$juveniles_at_chipps
      }

      # migrants at golden gate is a init adults(hatchery+natural) -> juvs -> survival+migration -> ocean

      ocean_entry_success <- ocean_entry_success(migrants = migrants_at_golden_gate,
                                                 month = month,
                                                 avg_ocean_transition_month = avg_ocean_transition_month,
                                                 .ocean_entry_success_length = ..params$.ocean_entry_success_length,
                                                 ..ocean_entry_success_int = ..params$..ocean_entry_success_int,
                                                 .ocean_entry_success_months = ..params$.ocean_entry_success_months,
                                                 stochastic = stochastic)

      adults_in_ocean <- adults_in_ocean + ocean_entry_success

      # # For use in the r2r metrics ---------------------------------------------
      d <- data.frame(juveniles_at_chipps)
      colnames(d) <- c("s", "m", "l", "vl")
      d$watershed <- fallRunDSM::watershed_labels
      d <- d |> tidyr::pivot_longer(names_to = "size",
                                    values_to = "juveniles_at_chipps", -watershed)
      d$year <- year
      d$month <- month
      output$juveniles_at_chipps <- dplyr::bind_rows(output$juveniles_at_chipps, d)
      # end R2R metric -----------------------------------------------------------
    } # end month loop

    output$juvenile_biomass[ , year] <- juveniles_at_chipps %*% fallRunDSM::params$mass_by_size_class
    # Updated logic here for R2R so that natural adults and hatchery adults return separately
    natural_adults_returning <- t(sapply(1:31, function(i) {
      if (stochastic) {
        rmultinom(1, (adults_in_ocean[i]), prob = c(.22, .47, .26, .05))
      } else {
        round((adults_in_ocean[i])* c(.22, .47, .26, .05))
      }
    })) * output$proportion_natural_juves_in_tribs[ , year]

    natural_adults_returning[is.na(natural_adults_returning)] = NaN

   # R2R release at chipps logic
    # TODO(stray) capture parameter for calculating straying
    # stray_rates_in_bay_releases <- hatchery_adult_stray()
    hatchery_releases_at_chipps <- ocean_entry_success(migrants = ..params$hatchery_releases_at_chipps,
                                               month = 8, # set to final month
                                               avg_ocean_transition_month = avg_ocean_transition_month,
                                               .ocean_entry_success_length = ..params$.ocean_entry_success_length,
                                               ..ocean_entry_success_int = ..params$..ocean_entry_success_int,
                                               .ocean_entry_success_months = ..params$.ocean_entry_success_months,
                                               stochastic = stochastic)

   hatchery_adults_returning <- t(sapply(1:31, function(i) {
     if (stochastic) {
       rmultinom(1, (adults_in_ocean[i]), prob = c(.30, .60, .10)) * (1 - output$proportion_natural_juves_in_tribs[ , year][i]) +
         rmultinom(1, (hatchery_releases_at_chipps[i]), prob = c(.30, .60, .10))
       } else {
         round((adults_in_ocean[i] * c(.30, .60, .10)) * (1 - output$proportion_natural_juves_in_tribs[, year][i])) +
           round((hatchery_releases_at_chipps[i]) * c(.30, .60, .10))}
     }))

   hatchery_adults_returning[is.na(hatchery_adults_returning)] = NaN

    # # For use in the r2r metrics ---------------------------------------------
    colnames(natural_adults_returning) <- c("V1", "V2", "V3", "V4")
    colnames(hatchery_adults_returning) <- c("V1", "V2", "V3")

    output$returning_adults <- bind_rows(
      output$returning_adults,
      natural_adults_returning |>
        dplyr::as_tibble(.name_repair = "universal") |>
        dplyr::mutate(watershed = watershed_labels,
               sim_year = year,
               origin = "natural") |>
        tidyr::pivot_longer(V1:V4, names_to = "return_year", values_to = "return_total") |>
        dplyr::mutate(return_sim_year = readr::parse_number(return_year) + 1 + as.numeric(sim_year)),
      hatchery_adults_returning |>
        dplyr::as_tibble(.name_repair = "universal") |>
        dplyr::mutate(watershed = watershed_labels,
               sim_year = year,
               origin = "hatchery") |>
        tidyr::pivot_longer(V1:V3, names_to = "return_year", values_to = "return_total") |>
        dplyr::mutate(return_sim_year = readr::parse_number(return_year) + 1 + as.numeric(sim_year))
    )
    # End R2R metric logic -----------------------------------------------------

    output$adults_in_ocean[,year] <- adults_in_ocean

    # distribute returning adults for future spawning
    if (mode == "calibrate") {
      calculated_adults[1:31, (year + 1):(year + 4)] <- calculated_adults[1:31, (year + 1):(year + 4)] + natural_adults_returning
      calculated_adults[1:31, (year + 1):(year + 3)] <- calculated_adults[1:31, (year + 1):(year + 3)] + hatchery_adults_returning
    } else {
      adults[1:31, (year + 1):(year + 4)] <- adults[1:31, (year + 1):(year + 4)] + natural_adults_returning
      adults[is.na(adults)] = 0
    }

  } # end year for loop

  if (mode == "seed") {
    return(adults[ , 6:30])
  } else if (mode == "calibrate") {
    return(calculated_adults[, 6:20])
  }

  spawn_change <- sapply(1:19, function(year) {
    output$spawners[ , year] / (output$spawners[ , year + 1] + 1)
  })

  viable <- spawn_change >= 1 & output$proportion_natural_juves_in_tribs[ , -1] >= 0.9 & output$spawners[ , -1] >= 833

  output$viability_metrics <- sapply(1:4, function(group) {
    colSums(viable[which(fallRunDSM::params$diversity_group == group), ])
  })

  return(output)

}





