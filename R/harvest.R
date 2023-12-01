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
                           year = year,
                           terminal_hatchery_logic = c(T, F),
                           ocean_harvest_percentage, # same for all tribs
                           tributary_harvest_percentage, # can vary by trib
                           restrict_harvest_to_hatchery = c(F, T),
                           no_cohort_harvest_years = NULL, # maybe we can get rid of this
                           intelligent_habitat_harvest = c(F, T),
                           intelligent_crr_harvest = c(F, T)
){
  # TODO make sure that it works with terminal hatchery logic
  # helper data
  return_prop <- matrix(c(.30, .60, .10, 0, .22, .47, .26, .05), nrow = 2, ncol = 4,
                        dimnames = list(c("hatchery", "natural"),
                                        c("V1", "V2", "V3", "V4")))
  # TODO apply baseline bycatch or hook mortality 10%
  # TODO think about adding min spawn habitat into intelligent CRR
  # OCEAN HARVEST
  # Set no harvest adults based on no cohort years and no hatchery only harvest restrictions
  no_harvest_adults <- adult_df |>
    filter(return_sim_year == year) |>
    mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well? follow up with technical team
           no_harvest = ifelse(restrict_harvest_to_hatchery & origin == "natural", T, no_harvest),
           age = return_sim_year - sim_year) |>
    filter(no_harvest) |>
    group_by(watershed, origin, age) |>
    summarise(remaining_adults = round(sum(return_total, na.rm = TRUE), 0))

  # allow harvest in harvest region and of hatchery (or natural if also desired)
  # harvest older adults more than non
  harvest_adults <- adult_df |>
    filter(return_sim_year == year) |>
    mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well
           no_harvest = ifelse(restrict_harvest_to_hatchery & origin == "natural", T, no_harvest)) |>
    filter(!no_harvest) |>
    rowwise() |>
    mutate(num_adults_required_after_harvest = ifelse(intelligent_crr_harvest,
                                                      round(output$spawners[watershed, sim_year] *
                                                              return_prop[origin, return_year], 0), 0), # this is capturing total
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

  # TODO add in river sport & tribal harvest

  all_adults <- bind_rows(harvest_adults, no_harvest_adults) |>
    group_by(watershed, age) |>
    summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = remaining_adults) |>
    select(-watershed) |>
    as.matrix()
  rownames(all_adults) <- watershed_labels

  hatchery_adults <- bind_rows(harvest_adults, no_harvest_adults) |>
    filter(origin == "hatchery") |>
    group_by(watershed, age) |>
    summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = remaining_adults) |>
    select(-watershed) |>
    as.matrix()
  rownames(hatchery_adults) <- watershed_labels

  natural_adults <- bind_rows(harvest_adults, no_harvest_adults) |>
    filter(origin == "natural") |>
    group_by(watershed, age) |>
    summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    ungroup() |>
    pivot_wider(names_from = age, values_from = remaining_adults) |>
    select(-watershed) |>
    as.matrix()
  rownames(natural_adults) <- watershed_labels

  total_adults <- all_adults
  hatchery_adults <- hatchery_adults
  natural_adults <- natural_adults
  proportion_natural <- natural_adults / total_adults

  # change nan to 0 for non spawn regions
  # recalculate pHOS

  list(hatchery_adults = replace(hatchery_adults, is.nan(hatchery_adults), 0),
       natural_adults = replace(natural_adults, is.nan(natural_adults), 0),
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), 0))
}
