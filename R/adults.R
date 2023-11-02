# hatchery = c("coleman", "feather", "merced", "mokelumne", "nimbus"), # hatchery of origin
# dist_hatch = normalize_with_context(0:1599, data$dist_hatch), # hatchery to release site distance, this encodes BAY vs INRIVER releases?
# run_year = normalize_with_context(1990, data$run_year), # the run year, this should be normalized
# age = normalize_with_context(3, data$age), # hathcery fish 60% return at age = 3
# Total_N = normalize_with_context(200000, data$Total_N), # obtained from ..params$hatchery_release
# rel_month = normalize_with_context(1:12, data$rel_month), # released are done all at once in the mode
# flow.1011 = normalize_with_context(200, data$flow.1011), # median flow for Oct
# flow_discrep = normalize_with_context(-200, data$flow_discrep), # difference between flow.1011 and avg flow apr-may
# mean_PDO_retn = normalize_with_context(0, data$mean_PDO_retn) # use the dataset they use cached intot he model, vary by year

#' @title Adult Straying for Hatchery Origin Fish
#' @param hatchery of origin, must be one of `c("coleman", "feather", "merced", "mokelumne", "nimbus")`
#' @param distance_from_hatchery the distance in km from hathcery of origin to release location
#' @param run_year year of run
#' @param age age of fish
#' @param total_released total number of fish released at hatchery
#' @param released_month month of release
#' @param flow_oct_nov the median flow for October and November
#' @param flow_apr_may the median flow for April and May
#' @param mean_pdo_return PDO return
#' @md
hatchery_adult_stray <- function(hatchery = c("coleman", "feather", "merced", "mokelumne", "nimbus"),
                                 distance_from_hatchery, run_year, age, total_released, released_month,
                                 flow_oct_nov, flow_apr_may, mean_pdo_return) {

  flow_discrep <- flow_oct_nov - flow_apr_may

  new_data <- data.frame(
    hatchery = hatchery,
    dist_hatch = distance_from_hatchery,
    run_year = run_year,
    age = age,
    Total_N = total_released,
    rel_month = released_month,
    flow.1011 = flow_oct_nov,
    flow_discrep = flow_discrep,
    mean_PDO_retn = mean_pdo_return
  )

  predictions <- predict(fallRunDSM::hatchery_stray_betareg)

}


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

stray_returning_adults <- function(monthly_adult_returns,
                                   year,
                                   stochastic,
                                   month_return_proportions,
                                   wild, prop_flow_natal,
                                   south_delta_routed_watersheds,
                                   cc_gates_days_closed,
                                   prop_bay_trans = 0, prop_delta_trans = 0,
                                   .adult_stray_intercept = fallRunDSM::params$.adult_stray_intercept,
                                   .adult_stray_wild = fallRunDSM::params$.adult_stray_wild,
                                   .adult_stray_natal_flow = fallRunDSM::params$.adult_stray_natal_flow,
                                   .adult_stray_cross_channel_gates_closed = fallRunDSM::params$.adult_stray_cross_channel_gates_closed,
                                   .adult_stray_prop_bay_trans = fallRunDSM::params$.adult_stray_prop_bay_trans,
                                   .adult_stray_prop_delta_trans = fallRunDSM::params$.adult_stray_prop_delta_trans) {

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
