# Install and load necessary packages
install.packages("KFAS")
library(KFAS)

# Simulate or use your actual salmon population data
set.seed(123)
salmon_population <- rnorm(20, mean = 100, sd = 10)
salmon_population <- as.matrix(salmon_population)
# Define the state-space model
# Use measurement and transition equations from lindley paper
# Resource as well https://lbelzile.github.io/timeseRies/state-space-models-and-the-kalman-filter.html#exercise-1-dynamic-linear-model-for-the-nile-river-dataset
ss_model <- SSModel(salmon_population ~ SSMtrend(2, Q = list(matrix(NA), matrix(NA))))

# Fit the model using the Kalman filter
kf_fit <- fitSSM(ss_model, inits = c(0, 0, 0, 0), method = "BFGS")

# Extract estimated parameters
estimated_params <- coef(kf_fit)

# Print the estimated parameters
cat("Estimated mu:", estimated_params["mu"], "\n")
cat("Estimated sd_process:", estimated_params["sd_process"], "\n")
cat("Estimated sd_obs:", estimated_params["sd_obs"], "\n")

# EXAMPLE WITH AIR PASSANGERS
library(dlm)
data("AirPassengers")

y2 <- log(window(AirPassengers, end = c(1958)))
library(KFAS)
model <- SSModel(y2 ~ SSMtrend(2, Q = list(matrix(NA), matrix(NA))) + SSMseasonal(period = 12,
                                                                                  sea.type = "dummy", Q = NA), H = matrix(NA))
fit2 <- fitSSM(model, inits = c(0, 0, 0, 0), method = "BFGS")
# Maximum likelihood estimate for sigmas
exp(fit2$optim.out$par)
