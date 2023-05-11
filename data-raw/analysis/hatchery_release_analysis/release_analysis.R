library(tidyverse)

fall_releases <- readxl::read_excel(path = "data-raw/analysis/hatchery_release_analysis/CVFRChin_RELEASE DB_v3_POSTED030419 (1).xlsx", sheet = "DBTable1") |> glimpse()
winter_releases <- readxl::read_excel("data-raw/analysis/hatchery_release_analysis/TEMP_ACollins_230424_98-21WCSRelease.xlsx", sheet = "Data") |> glimpse()

# notes - summarize by river
# have option for hatchery release in river OR at chipps
# baseline will be based off numbers currently released in river

# Fall quick summary
summarized_fall_release <- fall_releases |> glimpse()
  filter(Release_year >= 1995) |>
  group_by(Release_year, Hatchery) |>
  summarise(total_release = sum(Total_N, na.rm = TRUE)) |>
  group_by(Hatchery) |>
  summarize(seventy_fifth_percentile = quantile(total_release, .75),
            max_release = max(total_release, na.rm = TRUE),
            median_release = median(total_release, na.rm = TRUE)) |>
  glimpse()


summarized_fall_release$Hatchery |> unique()
summarized_fall_release$Release_site |> unique()

pairs <- matrix(c("COL", "Coleman National Fish Hatchery", "Battle Creek",
                  "FEA", "Feather River Hatchery", "Feather River",
                  "MER", "Merced River Fish Facility", "Merced River",
                  "NIM", "Nimbus Fish Hatchery", "American River",
                  "MOK", "Mokelumne Hatchery", "Mokelumne River"),
                ncol=3,
                byrow=T,
                dimnames = list(c(), c("Hatchery", "hatchery_name", "river"))) |>
  as_tibble(.name_repair = "check_unique") |> glimpse()

summarized_release <- left_join(summarized_fall_release, pairs) |>
  select(watershed = river, release_number = seventy_fifth_percentile) |>
  right_join(watershed_attributes |> select(watershed, order)) |>
  arrange(order) |>
  mutate(release_number = ifelse(is.na(release_number), 0, release_number)) |>
  glimpse()

# use values to populate named vector
hatchery_release = matrix(0, nrow = 31, ncol = 4, dimnames = list(fallRunDSM::watershed_labels, c("s", "m", "l", "xl")))

# populate hatchery release as larger fish based on avg hatchery release data
hatchery_release[, 3] <- summarized_release$release_number

usethis::use_data(fall_hatchery_release, overwrite = TRUE)
# TODO document data, figure out how we want to store for different scenarios
# TODO decide if I want to move to cache data...

summary(summarized_fall_release)
# Annual releases range from 1,010,176 - 56,462,517 (mean: 29,612,486)
# # TODO look for production targets

# winter quick summary
summarized_winter_release <- winter_releases |>
  mutate(total_released = `ReleasedClip/Tag` + `ReleasedNoClip/Tag` + `ReleasedClip/NoTag`) |>
  group_by(year = lubridate::year(InitialReleaseDate)) |>
  summarise(total_release = sum(total_released, na.rm = TRUE)) |>
  glimpse()

