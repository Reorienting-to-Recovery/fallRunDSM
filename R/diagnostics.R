ranges <- list(
  egg_to_fry = c(.85, 1)
)



create_diagnostic_report <- function(test_results, ranges) {

  index_filter <- which(!(fallRunDSM::watershed_labels %in% fallRunDSM::non_spawn_regions))
  # egg tp fry
  egg_to_fry_results <- test_results$egg_to_fry[, index_filter]
  egg_to_fry_ranges <- ranges$egg_to_fry
  egg_to_fry_out_of_range <- as_tibble(which(egg_to_fry_results < egg_to_fry_ranges[1] | egg_to_fry_results > egg_to_fry_ranges[2], arr.ind = TRUE))
  x <- egg_to_fry_out_of_range |> left_join(fallRunDSM::watershed_attributes |> select(watershed, order), by=c("col"="order"))
  x$val <- egg_to_fry_results[x$row, x$col]

}

create_diagnostic_report(fall_run_test_results, ranges)
