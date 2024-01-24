# TODO remove harvest from get_spawning_adults()
# TODO add this harvest adults function to line 124
#' Harvest Adults Function
#'
#' This function calculates the total adult salmon harvest based on various parameters.
#' This function is called within the larger model.R file before stray rates and prespawn survival is applied.
#'
#' @param adult_df A data frame containing information about adult salmon.
#' @param year The target year for the harvest calculation.
#' @param ocean_harvest_percentage The percentage of harvest from the ocean, same for all tributaries.
#' @param tributary_harvest_percentage The percentage of harvest from tributaries, can vary by tributary.
#' @param restrict_harvest_to_hatchery A logical vector indicating whether to restrict harvest to hatcheries for each tributary.
#' @param no_cohort_harvest_years A vector of years where no cohort harvest is allowed.
#' @param intelligent_crr_harvest A logical vector indicating whether to apply intelligent harvest based on Coho Retention Rate (CRR).
#'
#' @return A numeric value representing the total adult salmon harvest.
#'
#' @export

harvest_adults <- function(adult_df,
                           spawner_df,
                           early_adults,
                           year = year,
                           spawn_habitat,
                           seed_proportion_hatchery,
                           default_natural_age_distribution,
                           default_hatchery_age_distribution,
                           terminal_hatchery_logic = c(T, F),
                           ocean_harvest_percentage, # same for all tribs
                           tributary_harvest_percentage, # can vary by trib
                           restrict_harvest_to_hatchery = c(F, T),
                           no_cohort_harvest_years = NULL, # maybe we can get rid of this
                           intelligent_habitat_harvest = c(F, T),
                           intelligent_crr_harvest = c(F, T),
                           crr_scaling = 2
){
  # helper data ----------------------------------------------------------------
  # years 1 - 5
  # Harvest
  if (year <= 5) {
    hatch_adults <- early_adults[, year] * seed_proportion_hatchery
    # Default to base harvest levels .57 most tribs
    adults_after_harvest <- hatch_adults * (1 - (ocean_harvest_percentage + tributary_harvest_percentage))
    hatch_after_harvest_by_age <- round(unname(adults_after_harvest) * as.matrix(default_hatchery_age_distribution[2:5]))
    row.names(hatch_after_harvest_by_age) = fallRunDSM::watershed_labels
    colnames(hatch_after_harvest_by_age) = c(2, 3, 4, 5)
    harvested_hatchery_adults <- hatch_adults - adults_after_harvest
    # NATURAL
    if (restrict_harvest_to_hatchery) {
      nat_adults <- early_adults[, year] * (1 - seed_proportion_hatchery) * .9 # hooking mortality
      natutal_adults_by_age <- round(unname(natural_adults[, year] ) * as.matrix(default_natural_age_distribution[2:5]))
      harvested_natural_adults = rep(0, 31)
    } else {
      nat_adults <- early_adults[, year] * (1 - seed_proportion_hatchery)
      natutal_adults_after_harvest <- nat_adults * (1 - (ocean_harvest_percentage + tributary_harvest_percentage))
      natutal_adults_by_age <- round(unname(natutal_adults_after_harvest) * as.matrix(default_natural_age_distribution[2:5]))
      harvested_natural_adults <- nat_adults - natutal_adults_after_harvest
    }
    row.names(natutal_adults_by_age) = fallRunDSM::watershed_labels
    colnames(natutal_adults_by_age) = c(2, 3, 4, 5)
    return_data_list <- list(hatchery_adults = hatch_after_harvest_by_age,
                             natural_adults = natutal_adults_by_age,
                             harvested_hatchery_adults = harvested_hatchery_adults,
                             harvested_natural_adults = harvested_natural_adults)
  } else {
  # years 6 and on
  return_prop <- matrix(c(.30, .60, .10, 0, .22, .47, .26, .05), nrow = 2, ncol = 4,
                        dimnames = list(c("hatchery", "natural"),
                                        c("V1", "V2", "V3", "V4")))

  min_spawn_habitat <- apply(spawn_habitat[ , 10:12, year], 1, min)
  capacity <- min_spawn_habitat / fallRunDSM::params$spawn_success_redd_size
  hab_capacity <- tibble(watershed = fallRunDSM::watershed_labels,
                         habitat_capacity = capacity)

  watershed_order <- tibble(watershed = watershed_attributes$watershed,
                            order = watershed_attributes$order)
  total_harvest <- rep(ocean_harvest_percentage, 31) + tributary_harvest_percentage
  # Apply harvest ---------------------------------------------------------------
  # Set no harvest adults based on no cohort years and no hatchery only harvest restrictions
  no_harvest_adults <- adult_df |>
    filter(return_sim_year == year) |>
    mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well? follow up with technical team
           no_harvest = ifelse(restrict_harvest_to_hatchery & origin == "natural", T, no_harvest),
           age = return_sim_year - sim_year) |>
    filter(no_harvest) |>
    group_by(watershed, origin, age) |>
    summarise(remaining_adults = round(sum(return_total, na.rm = TRUE) * .9, 0)) # multipiles by .9 to remove 10 percent bycatch/hook mortality (high estimate)

  ocean_and_trib_harvest_adults <- adult_df |>
    filter(return_sim_year == year) |>
    left_join(hab_capacity, by = "watershed") |>
    mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well
           no_harvest = ifelse(restrict_harvest_to_hatchery & origin == "natural", T, no_harvest)) |>
    filter(!no_harvest) |>
    rowwise() |>
    mutate(num_adults_required_after_harvest = ifelse(intelligent_crr_harvest,
                                                      round(spawner_df[watershed, sim_year] *
                                                              return_prop[origin, return_year] * crr_scaling, 0), 0), # this is capturing total
           age = return_sim_year - sim_year,
           trib_harvest = tributary_harvest_percentage[watershed],
           adults_after_harvest = case_when(age == 2 ~ round((return_total *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - trib_harvest), 0) ,
                                        age == 3 ~ round(((return_total *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - trib_harvest), 0),
                                        age == 4 ~ round((((return_total *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - trib_harvest), 0),
                                        age == 5 ~ round(((((return_total *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - ocean_harvest_percentage)) *
                                                           (1 - trib_harvest), 0)),
           utalize_crr_totals = ifelse(num_adults_required_after_harvest > adults_after_harvest & intelligent_crr_harvest, TRUE, FALSE),
           utalize_hab_totals = ifelse(habitat_capacity > adults_after_harvest & intelligent_habitat_harvest, TRUE, FALSE),
           remaining_adults = round(case_when(utalize_crr_totals ~ min(num_adults_required_after_harvest, return_total),
                                              utalize_hab_totals ~ min(habitat_capacity, return_total),
                                              T ~ adults_after_harvest), 0),
           actual_harvest = round(case_when(utalize_crr_totals ~ max(return_total - num_adults_required_after_harvest, 0),
                                            utalize_hab_totals ~ max(return_total - habitat_capacity, 0),
                                            T ~ max(return_total - adults_after_harvest, 0)), 0)) |>
    select(watershed, origin, age, remaining_adults, actual_harvest)
  # Set to always allow - apply to no harvest and harvest adults, this will break crr logic (TODO talk with rene about that)
  # Recombine and synthesis
  hatchery_adults <- bind_rows(no_harvest_adults,
                               ocean_and_trib_harvest_adults) |>
    filter(origin == "hatchery") |>
    group_by(watershed, age) |>
    summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = remaining_adults) |>
    left_join(watershed_order, by = "watershed") |>
    arrange(order) |>
    select(-watershed, -order) |>
    as.matrix()
  rownames(hatchery_adults) <- watershed_labels

  # harvested hatch_adults
  harvested_hatchery_adults <- bind_rows(no_harvest_adults,
                                         ocean_and_trib_harvest_adults) |>
    filter(origin == "hatchery") |>
    group_by(watershed, age) |>
    summarise(actual_harvest = round(sum(actual_harvest, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = actual_harvest) |>
    left_join(watershed_order, by = "watershed") |>
    arrange(order) |>
    select(-watershed, -order) |>
    as.matrix()
  rownames(harvested_hatchery_adults) <- watershed_labels

  # Nat adults remaining
  natural_adults <- bind_rows(no_harvest_adults,
                              ocean_and_trib_harvest_adults) |>
    filter(origin == "natural") |>
    group_by(watershed, age) |>
    summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = remaining_adults) |>
    left_join(watershed_order, by = "watershed") |>
    arrange(order) |>
    select(-watershed, -order) |>
    as.matrix()
  rownames(natural_adults) <- watershed_labels

  # harvested natural
  harvested_natural_adults <- bind_rows(no_harvest_adults,
                                        ocean_and_trib_harvest_adults) |>
    filter(origin == "natural") |>
    group_by(watershed, age) |>
    summarise(actual_harvest = round(sum(actual_harvest, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = actual_harvest) |>
    left_join(watershed_order, by = "watershed") |>
    arrange(order) |>
    select(-watershed, -order) |>
    as.matrix()
  rownames(harvested_hatchery_adults) <- watershed_labels

  # Total adults (non confirming arrays so have to do differently)
  total_adults <- bind_rows(no_harvest_adults,
                            ocean_and_trib_harvest_adults) |>
    group_by(watershed, age) |>
    summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = remaining_adults) |>
    left_join(watershed_order, by = "watershed") |>
    arrange(order) |>
    select(-watershed, -order) |>
    as.matrix()
  rownames(total_adults) <- watershed_labels

  # change nan to 0 for non spawn regions
  return_data_list <- list(hatchery_adults = replace(hatchery_adults, is.nan(hatchery_adults), 0),
                           natural_adults = replace(natural_adults, is.nan(natural_adults), 0),
                           harvested_hatchery_adults = harvested_hatchery_adults,
                           harvested_natural_adults = harvested_natural_adults)
  }
  return(return_data_list)
}
