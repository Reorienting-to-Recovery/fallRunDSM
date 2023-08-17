x <- sample(1:10000)
y <- sample(0:10, 10000, replace = TRUE)


microbenchmark::microbenchmark(
  a = {
    r2r_model_results <- fallRunDSM::fall_run_model(mode = "simulate", ..params = fallRunDSM::r_to_r_baseline_params,
                                                    seeds = r2r_seeds)
  },
  times = 10
)

# Juveniles output as array
# with dataframe
# Unit: seconds
# expr      min       lq    mean   median       uq      max neval
# a 1.713786 1.742949 1.81697 1.796287 1.835576 2.048021    10

# with array
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# a 1.665064 1.683562 1.709988 1.708781 1.732786 1.754309    10
