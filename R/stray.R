#' Apply Straying
#'
#' @param year
#' @param natural_adults
#' @param hatchery_adults
#' @param total_releases = ..params$hatchery_release,
#' @param release_month = 1,
#' @param flows_oct_nov = ..params$flows_oct_nov,
#' @param flows_apr_may = ..params$flows_apr_may,
#' @param monthly_mean_pdo = fallRunDSM::monthly_mean_pdo,
apply_straying <- function(year, natural_adults, hatchery_adults, total_releases,
                           release_month, flows_oct_nov, flows_apr_may, monthly_mean_pdo) {

  nat_adults_temp <- natural_adults |>
    pivot_wider(names_from = "age", values_from = "return_total") |>
    transmute( `2`, `3` = 0, `4` = 0, `5` = 0) |>
    as.matrix() |>
    `row.names<-`(watershed_labels)


  hatch_adults_temp <- hatchery_adults |>
    pivot_wider(names_from = "age", values_from = "return_total") |>
    transmute( `2`, `3` = 0, `4` = 0, `5` = 0) |>
    as.matrix() |>
    `row.names<-`(watershed_labels)

  # TODO just use the values that are generated as part of the hatchery calculation for the natural stray rates, this way we only make the calculation once
  # calculate the straying
  natural_stray_rates <- compute_adult_stray_rates(type = "natural", sim_year = year, total_releases = total_releases,
                                                   released_month = release_month, flows_oct_nov = flows_oct_nov, flows_apr_may = flows_apr_may,
                                                   mean_pdo_return = monthly_mean_pdo)

  hatchery_stray_rates <- compute_adult_stray_rates(type = "hatchery", sim_year = year, total_releases = total_releases,
                                                   released_month = release_month, flows_oct_nov = flows_oct_nov, flows_apr_may = flows_apr_may,
                                                   mean_pdo_return = monthly_mean_pdo)

  # apply stray to natural
  strayed_natural_adults <- round(nat_adults_temp * natural_stray_rates$natural)


  # apply stray to hatchery
  # prop of in river vs prop in bay releases
  in_bay_releases <- hatch_adults_temp * fallRunDSM::hatchery_release_proportion_bay
  in_river_releases <- hatch_adults_temp * (1 - fallRunDSM::hatchery_release_proportion_bay)

  strayed_hatchery_adults <- ceiling(in_bay_releases * hatchery_stray_rates$release_bay + in_river_releases * hatchery_stray_rates$release_river)

  # reallocate strays



}



#' @title Adult Straying for Hatchery Origin Fish
#' @description
#' Calculates stray rates for all hatchery originating fish.
#'
#' @param release_type river or bay representing where the hatchery fish were released
#' @param run_year year of run
#' @param age age of fish
#' @param released total number of fish released at hatchery
#' @param flow_oct_nov the median flow for October and November
#' @param flow_apr_may the median flow for April and May
#' @param mean_pdo_return PDO return
#' @export
#' @md
compute_adult_stray_rates <- function(type = c("natural", "hatchery"), sim_year,
                                      total_releases, released_month, flows_oct_nov, flows_apr_may, mean_pdo_return) {


  # create "newdata" for each of the hatcheries to be used in the prediction
  new_data <- map_df(names(fallRunDSM::hatchery_to_watershed_lookup), function(x) {

    # prepare initial data
    w <- fallRunDSM::hatchery_to_watershed_lookup[x]
    flow_10_11 <- flows_oct_nov[w, sim_year]
    flow_4_5 <- flows_apr_may[w, sim_year]
    releases <- sum(total_releases[w, ])
    pdo <- mean_pdo_return[mean_pdo_return$year == sim_year + 1979 & mean_pdo_return$month == 1, ]$PDO

    prepare_stray_model_data(hatchery = x, type = type, sim_year = sim_year, flow_oct_nov = flow_10_11, flow_apr_may = flow_4_5,
                             releases = releases, mean_PDO_return = pdo)
  })

  predictions <- predict(fallRunDSM::hatchery_stray_betareg, newdata = new_data)
  new_data$prediction <- predictions
  age_unorm <- if (type == "natural") rep(2:5, 5) else rep(2:5, 10)
  stray_type = if (type == "natural") "natural" else rep(rep(c("release bay", "release river"), each = 4), 5)

  stray_rates <- new_data |> transmute(
    sim_year = sim_year,
    watershed = fallRunDSM::hatchery_to_watershed_lookup[hatchery],
    age = age_unorm,
    stray_type = stray_type,
    stray_rate = prediction
  )

  stray_rates_to_matrix(stray_rates, type = type)
}

#' @title Normalize data with context data
#' @description
#' transform data to be normalized given data to calculate mean and standard deviation from
#' @keywords internal
normalize_with_context <- function(x, context_data) {
  (x - mean(context_data, na.rm = TRUE))/sd(context_data, na.rm = TRUE)
}

#' @title Normalize data with known params
#' @description
#' transform data to be normalized given the mean and standard deviation from the data
#' @keywords internal
normalize_with_params <- function(x, mean_val, sd_val) {
  (x - mean_val)/sd_val
}


#' @keywords internal
stray_rates_to_matrix <- function(data, type) {
  out <- vector(mode = "list")
  out$natural <- NA
  out$release_bay <- NA
  out$release_river <- NA

  if (type == "natural") {
    # natural origin fish
    out$natural <- data |>
      filter(watershed == "American River", stray_type == "natural") |>
      pivot_wider(names_from = "age", values_from = "stray_rate") |>
      slice(rep(1:n(), each = 31)) |>
      select(`2`:`5`) |>
      as.matrix() |>
      `row.names<-`(watershed_labels)
  } else {
    # rates dataframe to the matrix for bay hatchery
    out$release_bay <- data |>
      filter(stray_type == "release bay") |>
      pivot_wider(values_from = "stray_rate", names_from = "age") |>
      select(-sim_year, -stray_type) |>
      right_join(select(watershed_attributes, watershed, order)) |>
      arrange(order) |>
      mutate(across(everything(), \(x) ifelse(is.na(x), 0, x))) |>
      select(-watershed, -order) |>
      as.matrix() |>
      `row.names<-`(watershed_labels)

    # rates for river release fish
    out$release_river <- data |>
      filter(stray_type == "release river") |>
      pivot_wider(values_from = "stray_rate", names_from = "age") |>
      select(-sim_year, -stray_type) |>
      right_join(select(watershed_attributes, watershed, order)) |>
      arrange(order) |>
      mutate(across(everything(), \(x) ifelse(is.na(x), 0, x))) |>
      select(-watershed, -order) |>
      as.matrix() |>
      `row.names<-`(watershed_labels)
  }

  return(out)
}

