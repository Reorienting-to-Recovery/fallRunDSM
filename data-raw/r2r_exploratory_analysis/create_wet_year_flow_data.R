library(tidyverse)

# PICK FLOW YEAR 1993 - replace it everywhere
DSMflow::flows_cfs$biop_itp_2018_2019 |>
  select(date, `Upper Sacramento River`) |>
  mutate(scenario = "baseline") |>
  filter(year(date) == 1983) |>
  ggplot(aes(x = date, y = `Upper Sacramento River`)) +
  geom_hline(yintercept = 4308) +
  geom_hline(yintercept = 10700) +
  geom_line(color = "blue3") +
  theme_minimal()

# generate_model_inputs inputs that are all 1993

# Decay Rate
recovery_decay_rate <- DSMhabitat::spawning_decay_multiplier$biop_itp_2018_2019$fr
recovery_decay_rate[,,1:22] <- recovery_decay_rate[,, 5]
recovery_decay_rate

# FLOW VARIABLES ---------------------------------------------------------------
# Freeport flows
recovery_freeport_flows <- DSMflow::freeport_flow$biop_itp_2018_2019
recovery_freeport_flows[ , 1:21] <- recovery_freeport_flows[, 4]
recovery_freeport_flows

# Vernalis flows
recovery_vernalis_flows <- DSMflow::vernalis_flow$biop_itp_2018_2019
recovery_vernalis_flows[, 1:21] <- recovery_vernalis_flows[, 4]
recovery_vernalis_flows

# Stockton flows
recovery_stockton_flows <- DSMflow::stockton_flow$biop_itp_2018_2019
recovery_stockton_flows[, 1:21] <- recovery_stockton_flows[, 4]
recovery_stockton_flows

# EXPORTS
recovery_cvp_exports <- DSMflow::cvp_exports$biop_itp_2018_2019
recovery_cvp_exports[, 1:21] <- recovery_cvp_exports[, 4]
recovery_cvp_exports

recovery_swp_exports <- DSMflow::swp_exports$biop_itp_2018_2019
recovery_swp_exports[, 1:21] <- recovery_swp_exports[, 4]
recovery_swp_exports

# Proportion diverted
recovery_proportion_diverted <- DSMflow::proportion_diverted$biop_itp_2018_2019
recovery_proportion_diverted[,, 1:21] <- recovery_proportion_diverted[,, 4]
recovery_proportion_diverted

# total diverted
recovery_total_diverted <- DSMflow::total_diverted$biop_itp_2018_2019
recovery_total_diverted[,, 1:21] <- recovery_total_diverted[,, 4]
recovery_total_diverted

# Delta prop diverted
recovery_delta_prop_diverted <- DSMflow::delta_proportion_diverted$biop_itp_2018_2019
recovery_delta_prop_diverted[, 1:21, 1] <- recovery_delta_prop_diverted[,4, 1]
recovery_delta_prop_diverted[, 1:21, 2] <- recovery_delta_prop_diverted[,4, 2]
recovery_delta_prop_diverted

# delta total diverted
recovery_delta_total_diverted <- DSMflow::delta_total_diverted$biop_itp_2018_2019
recovery_delta_total_diverted[, 1:21, 1] <- recovery_delta_total_diverted[,4, 1]
recovery_delta_total_diverted[, 1:21, 2] <- recovery_delta_total_diverted[,4, 2]
recovery_delta_total_diverted

# proportion pulse
# TODO see if we can improve this one

# proportion flow natal
recovery_prop_flow_natal <- DSMflow::proportion_flow_natal$biop_itp_2018_2019
recovery_prop_flow_natal[, 1:22] <- recovery_prop_flow_natal[,5]
recovery_prop_flow_natal

# sac flows
recovery_upper_sac_flows <- DSMflow::upper_sacramento_flows$biop_itp_2018_2019
recovery_upper_sac_flows[, 1:21] <- recovery_upper_sac_flows[, 4]
recovery_upper_sac_flows

# delta inflows
recovery_delta_inflows <- DSMflow::delta_inflow$biop_itp_2018_201
recovery_delta_inflows[, 1:21, 1] <- recovery_delta_inflows[,4, 1]
recovery_delta_inflows[, 1:21, 2] <- recovery_delta_inflows[,4, 2]
recovery_delta_inflows

# bypass flows
recovery_bypass_flows <- DSMflow::proportion_flow_bypasses$biop_itp_2018_2019
recovery_bypass_flows[, 1:21, 1] <- recovery_bypass_flows[,4, 1]
recovery_bypass_flows[, 1:21, 2] <- recovery_bypass_flows[,4, 2]
recovery_bypass_flows

# gates overtopped
recovery_gates_overtopped <- DSMflow::gates_overtopped$biop_itp_2018_2019
recovery_gates_overtopped[, 1:21, 1] <- recovery_gates_overtopped[,4, 1]
recovery_gates_overtopped[, 1:21, 2] <- recovery_gates_overtopped[,4, 2]
recovery_gates_overtopped

# TEMPERATURE VARIABLES --------------------------------------------------------
# vernalis temps
recovery_vernalis_temps <- DSMtemperature::vernalis_temperature
recovery_vernalis_temps[, 4] <- ifelse(recovery_vernalis_temps[, 4] > 17, 17, recovery_vernalis_temps[, 4])
recovery_vernalis_temps[, 1:21] <- recovery_vernalis_temps[, 4]
recovery_vernalis_temps

# prisoner point temp
recovery_pp_temps <- DSMtemperature::prisoners_point_temperature
recovery_pp_temps[, 4] <- ifelse(recovery_pp_temps[, 4] > 17, 17, recovery_pp_temps[, 4])
recovery_pp_temps[, 1:21] <- recovery_pp_temps[, 4]
recovery_pp_temps

# degree days
recovery_dd_temps <- DSMtemperature::degree_days$biop_itp_2018_2019
recovery_dd_temps[,, 1:22] <- recovery_dd_temps[,, 5]
recovery_dd_temps

# stream temps
recovery_stream_temps <- DSMtemperature::stream_temperature$biop_itp_2018_2019
recovery_stream_temps[,, 4] <- ifelse(recovery_stream_temps[,, 4] > 17, 17, recovery_stream_temps[,, 4])
recovery_stream_temps[,, 1:21] <- recovery_stream_temps[,, 4]
recovery_stream_temps

# delta temps
recovery_delta_temps <- DSMtemperature::delta_temperature
recovery_delta_temps[, 1:21, 1] <- recovery_delta_temps[,4, 1]
recovery_delta_temps[, 1:21, 2] <- recovery_delta_temps[,4, 2]
recovery_delta_temps

# HABITAT ----------------------------------------------------------------------
# spawn habitat
recovery_spawn_hab <- DSMhabitat::fr_spawn$r_to_r_tmh
recovery_spawn_hab[,, 1:22] <- recovery_spawn_hab[,, 5]
recovery_spawn_hab

# fry hab
recovery_fry_hab <- DSMhabitat::fr_fry$r_to_r_tmh
recovery_fry_hab[,, 1:21] <- recovery_fry_hab[,, 4]
recovery_fry_hab

# juv hab
recovery_juv_hab <- DSMhabitat::fr_juv$r_to_r_tmh
recovery_juv_hab[,, 1:21] <- recovery_juv_hab[,, 4]
recovery_juv_hab

# fp hab
recovery_fp_hab <- DSMhabitat::fr_fp$r_to_r_tmh
recovery_fp_hab[,, 1:21] <- recovery_fp_hab[,, 4]
recovery_fp_hab

# weeks flooded
recovery_weeks_flooded <- DSMhabitat::weeks_flooded$biop_itp_2018_2019
recovery_weeks_flooded[,, 1:21] <- recovery_weeks_flooded[,, 4]
recovery_weeks_flooded

# delta hab
recovery_delta_habitat <- DSMhabitat::delta_habitat$r_to_r_tmh
recovery_delta_habitat[, 1:21, 1] <- recovery_delta_habitat[,4, 1]
recovery_delta_habitat[, 1:21, 2] <- recovery_delta_habitat[,4, 2]
recovery_delta_habitat

# sutter habitat
recovery_sutter_hab <- DSMhabitat::sutter_habitat$biop_itp_2018_2019
recovery_sutter_hab[, 1:21] <- recovery_sutter_hab[,4]
recovery_sutter_hab

# sutter habitat
recovery_yolo_hab <- DSMhabitat::yolo_habitat$biop_itp_2018_2019
recovery_yolo_hab[, 1:21] <- recovery_yolo_hab[,4]
recovery_yolo_hab





