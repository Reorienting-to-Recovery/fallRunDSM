



#' @title Adult Straying
#' @description Calculate the proportion of adults straying to non-natal streams to spawn
#' @details See \code{\link{params}} for details on parameter sources
#' @param wild Variable indicator of wild fish returning
#' @param natal_flow Variable describing proportion flows at tributary junctions coming from natal watershed in October
#' @param south_delta_watershed Variable indicator if watershed feeds into South Delta
#' @param cross_channel_gates_closed Variable describing number of days gates are closed for each month
#' @param prop_bay_trans Variable describing proportion transport to the bay
#' @param prop_delta_trans Variable describing proportion transport to the delta
#' @param .intercept Intercept
#' @param .wild Coefficient for \code{wild} variable
#' @param .natal_flow Coefficient for \code{natal_flow} variable
#' @param .cross_channel_gates_closed Coefficient for \code{cross_channel_gates_closed} variable
#' @param .prop_bay_trans Coefficient for \code{prop_bay_trans} variable
#' @param .prop_delta_trans Coefficient for \code{prop_delta_trans} variable
#' @source IP-117068
#' @export
adult_stray <- function(wild, natal_flow, south_delta_watershed, cross_channel_gates_closed,
                        prop_bay_trans = 0, prop_delta_trans = 0, # bring these up top level so we can have as params based on hatchery logic (can also manipualate props in R2R scenario)
                        .intercept = fallRunDSM::params$.adult_stray_intercept,
                        .wild = fallRunDSM::params$.adult_stray_wild,
                        .natal_flow = fallRunDSM::params$.adult_stray_natal_flow,
                        .cross_channel_gates_closed = fallRunDSM::params$.adult_stray_cross_channel_gates_closed,
                        .prop_bay_trans = fallRunDSM::params$.adult_stray_prop_bay_trans,
                        .prop_delta_trans = fallRunDSM::params$.adult_stray_prop_delta_trans){

  boot::inv.logit(
    .intercept +
    .wild * wild +
    .natal_flow * natal_flow +
    .cross_channel_gates_closed * south_delta_watershed * cross_channel_gates_closed +
    .prop_bay_trans * prop_bay_trans * ( 1 - wild) +
    .prop_delta_trans * prop_delta_trans * (1 - wild)
  )

}

#' Apply Straying to Returning Adults
stray_returning_adults <- function(monthly_adult_returns,
                                   year,
                                   stochastic,
                                   month_return_proportions,
                                   wild, prop_flow_natal,
                                   south_delta_routed_watersheds,
                                   cc_gates_days_closed,
                                   prop_bay_trans = 0, prop_delta_trans = 0,
                                   type,
                                   total_releases,
                                   release_month,
                                   flows_oct_nov,
                                   flows_apr_may,
                                   monthly_mean_pdo,
                                   .adult_stray_intercept = fallRunDSM::params$.adult_stray_intercept,
                                   .adult_stray_wild = fallRunDSM::params$.adult_stray_wild,
                                   .adult_stray_natal_flow = fallRunDSM::params$.adult_stray_natal_flow,
                                   .adult_stray_cross_channel_gates_closed = fallRunDSM::params$.adult_stray_cross_channel_gates_closed,
                                   .adult_stray_prop_bay_trans = fallRunDSM::params$.adult_stray_prop_bay_trans,
                                   .adult_stray_prop_delta_trans = fallRunDSM::params$.adult_stray_prop_delta_trans) {


  stray_rates <- compute_adult_stray_rates(type = type, sim_year = year, total_releases = total_releases,
                                           released_month = release_month, flows_oct_nov = flows_oct_nov, flows_apr_may = flows_apr_may,
                                           mean_pdo_return = monthly_mean_pdo)

  stray_props <- sapply(10:12, function(month) {
    #TODO add different stray logic for realease types
    adult_stray(wild = wild,
                natal_flow = prop_flow_natal[ , year],
                south_delta_watershed = south_delta_routed_watersheds,
                cross_channel_gates_closed = cc_gates_days_closed[month],
                .intercept = .adult_stray_intercept,
                .wild = .adult_stray_wild,
                .natal_flow = .adult_stray_natal_flow,
                .cross_channel_gates_closed = .adult_stray_cross_channel_gates_closed,
                .prop_bay_trans = .adult_stray_prop_bay_trans,
                .prop_delta_trans = .adult_stray_prop_delta_trans)
  })

  straying_adults <- sapply(1:3, function(month) {
    if (stochastic) {
      rbinom(n = 31, monthly_adult_returns[, month], stray_props[, month])
    } else {
      round(monthly_adult_returns[, month] * stray_props[, month])
    }
  })

  south_delta_routed_adults <- round(colSums(straying_adults * south_delta_routed_watersheds))
  south_delta_stray_adults <- sapply(1:3, function(month) {
    if (stochastic) {
      as.vector(rmultinom(1, south_delta_routed_adults[month], cross_channel_stray_rate))
    } else {
      round(south_delta_routed_adults[month] * cross_channel_stray_rate)
    }
  })

  remaining_stray_adults <- round(colSums(straying_adults * (1 - south_delta_routed_watersheds)))
  stray_adults <- sapply(1:3, function(month) {
    if (stochastic) {
      as.vector(rmultinom(1, remaining_stray_adults[month], stray_rate))
    } else {
      round(remaining_stray_adults[month] * stray_rate)
    }
  })

  adults_after_stray <- monthly_adult_returns - straying_adults + south_delta_stray_adults + stray_adults
}

#' @title Adult En Route Survival
#' @description Calculate adult survival en route to spawning grounds
#' @details See \code{\link{params}} for details on parameter sources
#' @param migratory_temp Variable representing proportion of migratory corridor temperature above  20°C
#' @param bypass_overtopped Indicator for bypass overtopped
#' @param adult_harvest Adult harvest rate
#' @param ..surv_adult_enroute_int Intercept
#' @param .migratory_temp Coefficient for \code{migratory_temp} variable
#' @param .bypass_overtopped Coefficient for \code{bypass_overtopped} variable
#' @source IP-117068
#' @export

surv_adult_enroute <- function(migratory_temp, bypass_overtopped, adult_harvest,
                               ..surv_adult_enroute_int = fallRunDSM::params$..surv_adult_enroute_int,
                               .migratory_temp = fallRunDSM::params$.adult_en_route_migratory_temp,
                               .bypass_overtopped = fallRunDSM::params$.adult_en_route_bypass_overtopped) {

  pmax(boot::inv.logit(..surv_adult_enroute_int +
                       .migratory_temp * migratory_temp +
                       .bypass_overtopped * bypass_overtopped) - adult_harvest, 0)
}

#' @title Adult Prespawn Survival
#' @description Calculate the adult prespawn survival
#' @details See \code{\link{params}} for details on parameter sources
#' @param deg_day Variable describing average degree days
#' @param ..surv_adult_prespawn_int Intercept
#' @param .deg_day Coefficient for \code{deg_day} variable
#' @source IP-117068
#' @export
surv_adult_prespawn <- function(deg_day,
                                ..surv_adult_prespawn_int = fallRunDSM::params$..surv_adult_prespawn_int,
                                .deg_day = fallRunDSM::params$.adult_prespawn_deg_day){

  boot::inv.logit(..surv_adult_prespawn_int + .deg_day * deg_day)
}



#' @title Prepare Beta-regression Data
#' @description
#' Create data frame of model data to be used in the beta-regression model for stray rates
#' @param hatchery name of hatchery to produce data for
#' @param type either in river or bay release
#' @param sim_year simulation year, used to calculate run year
#' @param flow_oct_nov the mean flow for Oct-Nov, use DSMflow::hatchery_oct_nov_flows
#' @param flow_apr_may the mean flow for Apr-May, use DSMflow::hatchery_apr_may_flows
#' @param released the total number of released hatchery fish
#' @param mean_PDO_return ...
#'
#' @details
#' Natural origin fish get a stray rate equal to Feather stray at distance = 0.
#'
#' @keywords internal
prepare_stray_model_data <- function(hatchery, type = c("natural", "hatchery"), sim_year,
                                     flow_oct_nov, flow_apr_may, releases, mean_PDO_return) {
  run_year <- sim_year + 1980
  flow_discrep <- flow_oct_nov - flow_apr_may
  return(
    expand_grid(
      hatchery = hatchery,
      dist_hatch = normalize_with_params(
        if (type == "hatchery") c(fallRunDSM::hatchery_to_bay_distance[hatchery], 0) else  0,
        betareg_normalizing_context$dist_hatch$mean,
        betareg_normalizing_context$dist_hatch$sd),
      run_year = normalize_with_params(
        run_year, betareg_normalizing_context$run_year$mean, betareg_normalizing_context$run_year$sd
      ),
      age = normalize_with_params(2:5, betareg_normalizing_context$age$mean, betareg_normalizing_context$age$sd),
      Total_N = normalize_with_params(releases, betareg_normalizing_context$Total_N$mean, betareg_normalizing_context$Total_N$sd),
      rel_month = normalize_with_params(1, betareg_normalizing_context$rel_month$mean, betareg_normalizing_context$rel_month$sd),
      flow.1011 = normalize_with_params(flow_oct_nov, betareg_normalizing_context$flow.1011$mean, betareg_normalizing_context$flow.1011$sd),
      flow_discrep = normalize_with_params(
        flow_discrep,
        betareg_normalizing_context$flow_discrep$mean,
        betareg_normalizing_context$flow_discrep$sd),
      mean_PDO_retn = normalize_with_params(mean_PDO_return, betareg_normalizing_context$mean_PDO_retn$mean, betareg_normalizing_context$mean_PDO_retn$sd)
    )
  )
}




