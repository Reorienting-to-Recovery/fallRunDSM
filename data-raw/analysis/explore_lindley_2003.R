# library(KFAS)
library(tidyverse)
library(DSMCalibrationData)
library(dlm)

# build DLM function based on Lindley 2003 ----------------------------------------------------
# map matrices from paper (A, Q, c, e_t) to matrices in DLM (F, V, G, W)

lindley_fn <- function(pars) {
  # pars is vector of length 3 with the 3 parameters you're estimating
  # mu, measurement error, process error

  # state vector x_t is [Xt, mu]' (so 2 row x 1 col matrix)
  p = 2 # dimensions
  mu <- pars[1]
  sigma_m <- pars[2] # measurement error
  sigma_p <- pars[3] # process error

  # transition equation
  # x_t = Ax_t + n_t
  A <- matrix(c(1, 0, 1, 1), nrow = 2) # transition matrix, called G in traditional dlms, dimensions p x p
  Q <- matrix(c(sigma_p^2, 0, 0, 0), nrow = p) # variance-covariance matrix for process error

  # measurement equation
  # y_t = cx_t + e_t
  c <- t(matrix(c(1, 0))) # row vector, called F in traditional dlms, dimensions 1 x p
  e_t <- matrix(sigma_m^2) # e_t, or v_t in traditional dlms (dimension 1 x 1, or m x m)

  initial_state_mean <- c(8.751158, mu) # x0 = vector [X0, mu]
  initial_state_covariance <- matrix(c((1.1315914), 0, 0, 0), ncol = p)

  # map
  inputs <- list(FF = c,
                 V = e_t,
                 GG = A,
                 W = Q, # formerly n_t
                 m0 = initial_state_mean,
                 C0 = initial_state_covariance)

  # build dlm object
  my_dlm <- dlm(inputs)

  return(my_dlm)
}

# fit model ---------------------------------------------------------------

# set up data
ts <- DSMCalibrationData::grandtab_imputed$fall

# fit feather
# log transform and turn into matrix
X <- matrix(log(ts["American River",]))

# initial par values
initial_pars <- c(mean(X), 1, 1) # using 0.1 as default for process/measurement sigmas

# build model for MLE
lindley_model <- dlmMLE(X, parm = initial_pars, build = lindley_fn)

# use par estimates to fit the recursive function
model_fit <- lindley_fn(lindley_model$par)

# apply Kalman filter
filtered_model_fit <- dlmFilter(X, model_fit)

# posterior
filtered_model_fit$m

# state at time step (which is X_t and mu_t)
filtered_model_fit$a

estimated_mu <- unique(filtered_model_fit$a[,2])
estimated_process_error <- filtered_model_fit$mod$W[1,1]
estimated_measurement_error <- filtered_model_fit$mod$V[1,1]

model_fit <- tibble("year" = as.numeric(years),
                    "log_abundance" = as.numeric(X),
                    "mu" = estimated_mu,
                    "PE" = estimated_process_error,
                    "ME" = estimated_measurement_error) |>
  mutate("predicted_log_abundance" = lag(log_abundance) + mu + PE,
         "state_space_growth_rate" = mu + rnorm(1, 0, PE) + rnorm(1, 0, ME))

# next steps
# TODO model diagnostic reporting
# TODO hessian / std errors on parameter estimates
# TODO plot PE by tributary; relative natural variability
# TODO informative priors for measurement errors and/or keep measurement error is the same across tribs

# first pass compare mu across watersheds ---------------------------------
watersheds <- fallRunDSM::watershed_labels
years <- as.numeric(names(DSMCalibrationData::grandtab_observed$fall[1,]))

get_pop_rate_watershed <- function(watershed_name) {
  cli::cli_bullets(paste0(watershed_name))
  if(sum(ts[watershed_name,]) == 0) {
    # catch any watersheds where all the abundance counts are 0
    return(tibble("years" = years,
                  "watershed" = watershed_name,
                  "lindley_growth_rate" = NA))
  }

  # fit the lindley model
  X <- matrix(log(ts[watershed_name,]))
  initial_pars <- c(mean(X), 1, 1) # using 0.1 as default for process/measurement sigmas
  lindley_model <- dlmMLE(X, parm = initial_pars, build = lindley_fn)
  model_fit <- lindley_fn(lindley_model$par)
  filtered_model_fit <- dlmFilter(X, model_fit)

  mu <- unique(filtered_model_fit$a[,2])

  # only return mu (lots more to get out of this model)
  time_series <- tibble(years) |>
    mutate(watershed = watershed_name,
           lindley_growth_rate = mu)
  return(time_series)
}

# run the lindley model on all watershed time series
all_watersheds <- purrr::map(watersheds, get_pop_rate_watershed) |>
  bind_rows()

all_watersheds |>
  ggplot(aes(x = watershed, y = lindley_growth_rate)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# scratch ---------------------------------------------------

# original approach (pop rate instantaneous)
data_frame <- tibble("abundance" = ts,
                     "year" = as.numeric(years)) |>
  mutate(lag = lag(abundance, n = 1),
         growth_rate = (abundance - lag)/abundance)

compare <- tibble("abundance" = ts,
                  "year" = as.numeric(years),
                  "original_growth_rate" = data_frame$growth_rate,
                  "lindley_growth_rate" = model_fit$mu,
                  "lindley_ss_growth_rate" = model_fit$state_space_growth_rate)

mean_pop_growth_rate <- mean(data_frame$growth_rate, na.rm = T)

compare |>
  ggplot(aes(x = year, y = original_growth_rate)) +
  geom_line() +
  geom_line(aes(x = year, y = lindley_ss_growth_rate), color = "red")


# references
# https://quant.stackexchange.com/questions/59333/dlm-package-in-r-to-estimate-a-state-space-model-with-drifts
# https://www.math.unm.edu/~ghuerta/tseries/dlmch2.pdf
# https://cran.r-project.org/web/packages/dlm/dlm.pdf
# https://mjlaine.github.io/dlm/dlmtut.html
# https://mc-stan.org/docs/2_34/functions-reference/gaussian-dynamic-linear-models.html
# http://lalas.github.io/quantitativeThoughts/r/2014/09/01/dlmTutorial.html

# try for basic SStrend ---------------------------------------------------
X <- log(ts)  # log transformed state

model <- SSModel(X ~ SSMtrend(degree = 1, ))

model <- SSModel(X ~ SSMtrend(degree = 1, Q = list(Q)))

fit <- fitSSM(model, inits = c(0), method = "BFGS")
kalman_fit <- KFS(fit$model)


# -------------------------------------------------------------------------

df <- tibble("x" = 1:20,
             "y" = X)
df |>
  ggplot(aes(x = x, y = y)) + geom_point() +
  geom_smooth(method = "lm")
# try for Lindley 2003 ----------------------------------------------------

X <- log(ts)
x <- matrix(c()) # TODO fill in state vector
r <- NA # measurement error
c <- matrix(c(0, 1)) # TODO transpose?
Q <- matrix(c(NA, 0, 0, 0), nrow = 2) # variance-covariance matrix for process error
A <- matrix(c(1, 1, 0, 1), nrow = 2) # transition matrix

Z_fn <- function(x_state) {
  P_t <- # TODO fill this in
}
T_fn <- function(x_state) {
  pred_x <- x_state[1,1] %*% A # predicts tate by propogating previous state through transition matrix
  pred_x
}

lindley_model <- SSModel(X ~ SSMcustom(Z, T, R, Q, a1, P1, P1inf, index, n = 1, state_names))


