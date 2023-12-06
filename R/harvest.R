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
                           year = year,
                           spawn_habitat,
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
  return_prop <- matrix(c(.30, .60, .10, 0, .22, .47, .26, .05), nrow = 2, ncol = 4,
                        dimnames = list(c("hatchery", "natural"),
                                        c("V1", "V2", "V3", "V4")))

  min_spawn_habitat <- apply(spawn_habitat[ , 10:12, year], 1, min)
  capacity <- min_spawn_habitat / fallRunDSM::params$spawn_success_redd_size
  hab_capacity <- tibble(watershed = fallRunDSM::watershed_labels,
                         capacity = capacity)

  watershed_order <- tibble(watershed = watershed_attributes$watershed,
                            order = watershed_attributes$order)
  # Apply ocean harvest --------------------------------------------------------
  # TODO think about adding min spawn habitat into intelligent CRR
  # OCEAN HARVEST
  # Set no harvest adultBs based on no cohort years and no hatchery only harvest restrictions
  no_harvest_adults <- adult_df |>
    filter(return_sim_year == year) |>
    mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well? follow up with technical team
           no_harvest = ifelse(restrict_harvest_to_hatchery & origin == "natural", T, no_harvest),
           no_harvest = ifelse(terminal_hatchery_logic, T, F),
           age = return_sim_year - sim_year) |>
    filter(no_harvest) |>
    group_by(watershed, origin, age) |>
    summarise(remaining_adults = round(sum(return_total, na.rm = TRUE) * .9, 0)) # multipiles by .9 to remove 10 percent bycatch/hook mortality (high estimate)

  if (intelligent_habitat_harvest) {
    # Create harvested adults for crr = true
    # ocean_harvested_adults <-
  } else {
  # allow harvest in harvest region and of hatchery (or natural if also desired)
  # harvest older adults more than non
    ocean_harvest_adults <- adult_df |>
      filter(return_sim_year == year) |>
      mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well
             no_harvest = ifelse(restrict_harvest_to_hatchery & origin == "natural", T, no_harvest),
             no_harvest = ifelse(terminal_hatchery_logic, T, F)) |>
      filter(!no_harvest) |>
      rowwise() |>
      mutate(num_adults_required_after_harvest = ifelse(intelligent_crr_harvest,
                                                        round(spawner_df[watershed, sim_year] *
                                                                return_prop[origin, return_year] * crr_scaling, 0), 0), # this is capturing total
             age = return_sim_year - sim_year,
             harvested_adults = case_when(age == 2 ~ round(return_total *
                                                             ocean_harvest_percentage, 0) ,
                                          age == 3 ~ round(return_total *
                                                             ocean_harvest_percentage *
                                                             ocean_harvest_percentage, 0),
                                          age == 4 ~ round(return_total *
                                                             ocean_harvest_percentage *
                                                             ocean_harvest_percentage *
                                                             ocean_harvest_percentage, 0),
                                          age == 5 ~ round(return_total *
                                                             ocean_harvest_percentage *
                                                             ocean_harvest_percentage *
                                                             ocean_harvest_percentage *
                                                             ocean_harvest_percentage, 0)),
             utalize_crr_totals = ifelse(num_adults_required_after_harvest < (return_total - harvested_adults) & intelligent_crr_harvest, TRUE, FALSE),
             remaining_adults = round(ifelse(utalize_crr_totals, num_adults_required_after_harvest, return_total - harvested_adults), 0),
             actual_harvest = round(ifelse(utalize_crr_totals, return_total - num_adults_required_after_harvest, harvested_adults), 0),) |>
      select(watershed, origin, age, remaining_adults)
  }
  # Set to always allow - apply to no harvest and harvest adults, this will break crr logic (TODO talk with rene about that)
  # Recombine and synthesis
  hatchery_adults <- bind_rows(no_harvest_adults,
                               ocean_harvest_adults) |>
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

  # Apply trib harvest (to all fish)
  hatch_adults_apply_trib_harvest <- round(hatchery_adults * (1 - tributary_harvest_percentage))

  natural_adults <- bind_rows(no_harvest_adults,
                              ocean_harvest_adults) |>
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

  # Apply trib harvest (to all fish)
  natural_adults_apply_trib_harvest <- round(natural_adults * (1 - tributary_harvest_percentage))

  # Total adults (non confirming arrays so have to do differently)
  total_adults <- bind_rows(no_harvest_adults,
                              ocean_harvest_adults) |>
    group_by(watershed, age) |>
    summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = remaining_adults) |>
    left_join(watershed_order, by = "watershed") |>
    arrange(order) |>
    select(-watershed, -order) |>
    as.matrix()
  rownames(total_adults) <- watershed_labels

  # Apply trib harvest (to all fish)
  total_adults_apply_trib_harvest <- round(total_adults * (1 - tributary_harvest_percentage))

  # prepare outpults
  total_adults <- total_adults_apply_trib_harvest
  hatchery_adults <- hatch_adults_apply_trib_harvest
  natural_adults <- natural_adults_apply_trib_harvest
  proportion_natural <- natural_adults_apply_trib_harvest / total_adults

  # change nan to 0 for non spawn regions
  # recalculate pHOS
  list(hatchery_adults = replace(hatchery_adults, is.nan(hatchery_adults), 0),
       natural_adults = replace(natural_adults, is.nan(natural_adults), 0),
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), 0))
}
