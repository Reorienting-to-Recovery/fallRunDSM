library(tidyverse)

# pull in hatchery data from https://www.pcouncil.org/documents/2022/03/2022-preseason-report-i.pdf/ table 11-1
raw_harvest_data <- read_csv("data-raw/analysis/harvest_analysis/harvest_data_raw.csv") |>
  glimpse()

harvest <- raw_harvest_data |>
  mutate(ocean_harvest_percentage = ocean_harvest_total/sacramento_index,
         river_harvest_percentage = river_harvest/sacramento_index) |>
  summarise(mean_ocean_harvest_prop = mean(ocean_harvest_percentage),
            mean_river_harvest_prop = mean(river_harvest_percentage)) |>
  glimpse()

# ocean harvest proportion = .5
# river harvest proportion = .8 (only apply to tribs with ocean harvest - Sacramento Tribs)
r2r_adult_harvest_rate = c(0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58,
                           0.58, 0.58, 0.58, 0.00, 0.00, 0.58, 0.5, 0.5, 0.00, 0.00, 0.58, 0.00, 0.5,
                           0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

names(r2r_adult_harvest_rate)<- fallRunDSM::watershed_labels
r2r_adult_harvest_rate
usethis::use_data(r2r_adult_harvest_rate, overwrite = TRUE)
