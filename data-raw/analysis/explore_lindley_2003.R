# test out Lindley 2003
library(KFAS)
library(tidyverse)
library(DSMCalibrationData)
library(dlm)

# get example time series data
ts <- unname(DSMCalibrationData::grandtab_observed$fall[1,])
years <- names(DSMCalibrationData::grandtab_observed$fall[1,])

# steps for fitting with Kalman filter
# define variables

# try for Lindley 2003 ----------------------------------------------------

lindley_fn <- function(pars) {

  # state vector x_t is [Xt, mu]' (so 2 row x 1 col matrix)
  p = 2
  mu <- pars[1]
  sigma_m <- pars[2] # measurement error
  sigma_p <- pars[3] # process error

  # transition equation
  # x_t = Ax_t + n_t
  A <- matrix(c(1, 0, 1, 1), nrow = 2) # transition matrix, called G in traditional dlms, dimensions p x p
  Q <- matrix(c(sigma_p^2, 0, 0, 0), nrow = p) # variance-covariance matrix for process error
  n_t <- t(matrix(rnorm(2, 0, Q))) # n, or w_t in traditional dlms, dimensions p x 1, evolution error

  # measurement equation
  # y_t = cx_t + e_t
  c <- t(matrix(c(1, 0))) # row vector, called F in traditional dlms, dimensions 1 x p
  e_t <- sigma_m^2 # e_t, or v_t in traditional dlms (dimension 1 x 1, or m x m)

  initial_state_mean <- c(8.751158, mu) # x0 = vector [X0, mu]
  #initial_covariance <- matrix(c())
  initial_state_covariance <- matrix(c((1.1315914), 0, 0, 0), ncol = p)

  inputs <- list(FF = c,
                 V = e_t,
                 GG = A,
                 W = Q, # formerly n_t
                 m0 = initial_state_mean,
                 C0 = initial_state_covariance)

  my_dlm <- dlm(inputs)

  return(my_dlm)
}

X <- matrix(log(ts)) # log scale time-series
# builds the model
lindley_model <- dlmMLE(X, parm = c(8, .1, .1), build = lindley_fn)
# use par estimates to fit the recursive function
model_fit <- lindley_fn(lindley_model$par)
# apply Kalman filter
filtered_model_fit <- dlmFilter(X, model_fit)

# posterior
filtered_model_fit$m

# state at time step (which is X_t and mu_t)
filtered_model_fit$a

# does this work?
data_frame <- tibble("abundance" = ts,
                     "year" = years) |>
  mutate(lag = lag(abundance, n = 1),
         growth_rate = (abundance - lag)/abundance)

mean_pop_growth_rate <- mean(data_frame$growth_rate, na.rm = T)


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


