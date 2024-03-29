% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rear.R
\name{rear}
\alias{rear}
\title{Rearing Function}
\source{
IP-117068
}
\usage{
rear(
  juveniles,
  survival_rate,
  growth,
  floodplain_juveniles = NULL,
  floodplain_survival_rate = NULL,
  floodplain_growth = NULL,
  weeks_flooded = NULL,
  stochastic
)
}
\arguments{
\item{juveniles}{Number of juveniles before growth or survival rates are applied
(inchannel for tributaries or total for bypasses and delta)}

\item{survival_rate}{Survival rate by size class (inchannel for tributaries or total for bypasses and delta)}

\item{growth}{Growth transition matrix for juveniles (inchannel for tributaries or total for bypasses and delta)}

\item{floodplain_juveniles}{Number of juveniles on the floodplain before growth or survival rates are applied (NULL for bypasses and delta)}

\item{floodplain_survival_rate}{Floodplain survival rate by size class (NULL for bypasses and delta)}

\item{floodplain_growth}{Growth transition matrix for juveniles rearing on the floodplain (NULL for bypasses and delta)}

\item{weeks_flooded}{Number of weeks floodplain is inundated (0 through 4)}

\item{stochastic}{\code{TRUE} \code{FALSE} value indicating if model is being run stochastically}
}
\description{
Calculates the number of juveniles that survive and grow inchannel and on the floodplain
}
