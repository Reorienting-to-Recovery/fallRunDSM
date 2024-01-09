library(tidyverse)
library(DSMCalibrationData)

adult_seeds <- matrix(0, nrow = 31, ncol = 30)
no_fr_spawn <- !as.logical(DSMhabitat::watershed_species_present[1:31, ]$fr *
              DSMhabitat::watershed_species_present[1:31,]$spawn)

adult_seed_values <- DSMCalibrationData::mean_escapement_2013_2017 %>%
  bind_cols(no_fr_spawn = no_fr_spawn) %>%
  select(watershed, Fall, no_fr_spawn) %>%
  mutate(corrected_fall = case_when(
    no_fr_spawn ~ 0,
    is.na(Fall) | Fall < 10 ~ 12,
    TRUE ~ Fall)
  ) %>% pull(corrected_fall)

adult_seeds[ , 1] <- adult_seed_values

rownames(adult_seeds) <- DSMhabitat::watershed_species_present$watershed_name[-32]
usethis::use_data(adult_seeds, overwrite = TRUE)

proportion_hatchery <- c(0.37, 0.2, 0.9, 0.37968253968254, 0.2, 0.115, 0.2225, 0.3525,
                         0.2, 0.16, 0.37968253968254, 0.1525, 0.365, 0.37968253968254,
                         0.37968253968254, 0.37968253968254, 0.37968253968254, 0, 0.855,
                         0.54, 0.37968253968254, 0.37968253968254, 0.571666666666667,
                         0.37968253968254, 0.0766666666666667, 0.02, 0.7575, 0.745, 0.705,
                         0.465, 0.37968253968254)

names(proportion_hatchery) <- DSMhabitat::watershed_metadata$watershed[-32]

usethis::use_data(proportion_hatchery, overwrite = TRUE)


# @title Hatchery Returns
# @export
# emanuel: not needed
# total_hatchery_returning <- round(stats::runif(1,83097.01, 532203.1)) # what is this

# @title Proportion of Adults Spawning October to December
# @export
month_return_proportions <- c(0.2222222,0.5555556,0.2222222)
names(month_return_proportions) <- month.abb[10:12]
usethis::use_data(month_return_proportions, overwrite = TRUE)

# Mass by size class
mass_by_size_class <- c(0.5, 1.8, 9.1, 31.4)
names(mass_by_size_class) <- c("s", "m", "l", "vl")
usethis::use_data(mass_by_size_class, overwrite = TRUE)

cross_channel_stray_rate <- c(rep(1, 15), 0, 0, 2, 2, 2, 0, 0, 3, 0, rep(0, 7)) / 24
names(cross_channel_stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(cross_channel_stray_rate, overwrite = TRUE)

stray_rate <- c(rep(1, 15), 0, 0, 1, 1, 1, 0, 0, 1, 0, rep(1, 6), 0) / 25
names(stray_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(stray_rate, overwrite = TRUE)


# differs based on run ------

adult_harvest_rate <- c(0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14, 0.14,
                        0.14, 0.14, 0.14, 0.14, 0.14, 0, 0, 0.14, 0.1, 0.1, 0, 0, 0.33,
                        0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0)
names(adult_harvest_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(adult_harvest_rate, overwrite = TRUE)

natural_adult_removal_rate <- c(0, 0, 0.7456033, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0.2442951, 0, 0, 0, 0.2291128, 0, 0, 0, 0.3944915, 0.2737981,
                                0, 0, 0)  # differs based on run
names(natural_adult_removal_rate) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(natural_adult_removal_rate, overwrite = TRUE)


hatchery_allocation <- c(0.067203316, 0.000177541, 0.150929409, 0.000154172, 0.000177541,
                         0.000792731, 0.010619948, 0.008426863, 0.002838529, 0.001160381,
                         0.000154172, 0.000662392, 0.000177541, 0.000154172, 0.000154172,
                         0, 0, 0.000154172, 0.482868351, 0.077602736, 0, 0, 0.133611589,
                         0, 0.000296477, 0.000154172, 0.02819385, 0.013632706, 0.014822544,
                         0.004880526, 0) # differs based on run
names(hatchery_allocation) <- DSMhabitat::watershed_metadata$watershed[-32]
usethis::use_data(hatchery_allocation, overwrite = TRUE)


original_groups <- read_csv("data-raw/misc/Grouping.csv")

diversity_group <- original_groups$diversity_group
names(diversity_group) <- original_groups$watershed
usethis::use_data(diversity_group, overwrite = TRUE)



watershed_labels <- c("Upper Sacramento River", "Antelope Creek", "Battle Creek",
                      "Bear Creek", "Big Chico Creek", "Butte Creek", "Clear Creek",
                      "Cottonwood Creek", "Cow Creek", "Deer Creek", "Elder Creek",
                      "Mill Creek", "Paynes Creek", "Stony Creek", "Thomes Creek",
                      "Upper-mid Sacramento River", "Sutter Bypass", "Bear River",
                      "Feather River", "Yuba River", "Lower-mid Sacramento River",
                      "Yolo Bypass", "American River", "Lower Sacramento River", "Calaveras River",
                      "Cosumnes River", "Mokelumne River", "Merced River", "Stanislaus River",
                      "Tuolumne River", "San Joaquin River")

usethis::use_data(watershed_labels)

size_class_labels <- c('s', 'm', 'l', 'vl')

usethis::use_data(size_class_labels)

# calculate growth rates
growth_rates_inchannel <- growth()
usethis::use_data(growth_rates_inchannel)
growth_rates_floodplain <- growth_floodplain()
usethis::use_data(growth_rates_floodplain)

# cache new growth rates
bioenergetics_transitions <- read_rds("data-raw/growTPM.rds")
usethis::use_data(bioenergetics_transitions, overwrite = TRUE)


# prey_density <- rep("med", 31) # NOTE this is to drive the new prey density dependent growth
prey_density <- rep("med", 31) # NOTE this is to drive the new prey density dependent growth
usethis::use_data(prey_density, overwrite = TRUE)

# should be moved to a data package?
prey_density_delta <- c("med", "med")
usethis::use_data(prey_density_delta, overwrite = TRUE)

# watershed attributes
watershed_attributes <- read_csv("data-raw/watershed-attributes.csv")
usethis::use_data(watershed_attributes, overwrite = TRUE)

# R2R cache data objects
# see hatchery markdown/analysis and harvest analysis for data objects
# TODO eventually clean up and move into this script


# distances from hatchery to bay
hatchery_to_bay_distance <- c(445, 239, 138, 133, 270)
names(hatchery_to_bay_distance) <- c("coleman", "feather", "nimbus", "mokelumne", "merced")
usethis::use_data(hatchery_to_bay_distance, overwrite = TRUE)

all_data <- read_csv("data-raw/stray-eda/alldata_formodel_031918.csv")
betareg_normalizing_context <- all_data |>
  select(dist_hatch, run_year, age, Total_N, rel_month, flow.1011, flow_discrep, mean_PDO_retn) |>
  map(\(x) list("mean" = mean(x), "sd" = sd(x)))

usethis::use_data(betareg_normalizing_context, overwrite = TRUE)

hatchery_to_watershed_lookup <- c(
  "coleman" = "Battle Creek",
  "feather" = "Feather River",
  "nimbus" = "American River",
  "mokelumne" = "Mokelumne River",
  "merced" = "Merced River"
)
usethis::use_data(hatchery_to_watershed_lookup, overwrite = TRUE)

watershed_to_hatchery_lookup <- names(fallRunDSM::hatchery_to_watershed_lookup)
names(watershed_to_hatchery_lookup) <- fallRunDSM::hatchery_to_watershed_lookup

usethis::use_data(watershed_to_hatchery_lookup, overwrite = TRUE)


# proportion of in-river vs bay releases
hatchery_release_proportion_bay <- 0
# names(hatchery_release_proportion_bay1) <- fallRunDSM::watershed_labels
usethis::use_data(hatchery_release_proportion_bay, overwrite = TRUE)


# mean PDO
raw_ocean_pdo <- read_csv("data-raw/stray-eda/PDO_monthly_Mantua.csv", skip = 1, col_names = c("year", "month", "PDO"))
monthly_mean_pdo <- raw_ocean_pdo |>
  filter(year %in% 1980:2002) |>
  mutate(date = as_date(paste0(year, "-", month, "-01")))

usethis::use_data(monthly_mean_pdo, overwrite = TRUE)


straying_destinations <-
  matrix(nrow = 31, ncol = 6, dimnames = list(watershed_labels, c(names(fallRunDSM::hatchery_to_watershed_lookup), "default")))

default_prop_coleman <- ((100 - 35 - 13 - 12)/28)/100
default_prop_feather <- ((100 - 44 - 16 - 14)/28)/100
default_prop_nimbus <- ((100 - 41 - 18)/29)/100
default_prop_mokelumne <- ((100 - 44 - 21 )/29)/100
default_prop_merced <- ((100 - 25 - 20 - 14 )/28)/100

straying_destinations[, "coleman"] <- default_prop_coleman
straying_destinations["Feather River", "coleman"] <- .13
straying_destinations["Upper Sacramento River", "coleman"] <- .35
straying_destinations["Clear Creek", "coleman"] <- .12

straying_destinations[, "feather"] <- default_prop_feather
straying_destinations["Upper Sacramento River", "feather"] <- .44
straying_destinations["Yuba River", "feather"] <- .16
straying_destinations["Clear Creek", "feather"] <- .14

straying_destinations[, "nimbus"] <- default_prop_nimbus
straying_destinations["Mokelumne River", "nimbus"] <- .41
straying_destinations["Yuba River", "nimbus"] <- .18


straying_destinations[, "mokelumne"] <- default_prop_mokelumne
straying_destinations["American River", "mokelumne"] <- .44
straying_destinations["Stanislaus River", "mokelumne"] <- .21


straying_destinations[, "merced"] <- default_prop_merced
straying_destinations["Mokelumne River", "merced"] <- .25
straying_destinations["Stanislaus River", "merced"] <- .20
straying_destinations["American River", "merced"] <- .14

straying_destinations[, "default"] <- 1/31


# TODO make the non spawning locations be 0
usethis::use_data(straying_destinations, overwrite = TRUE)

natural_straying_destinations <- matrix(1/31, nrow = 31, ncol = 4)

rmultinom(n = 1, size = matrix(1:4, ncol = 2), prob = matrix(1:4, ncol = 2))


