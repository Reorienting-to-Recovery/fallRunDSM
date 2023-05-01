library(tidyverse)


fall_releases <- readxl::read_excel(path = "data-raw/analysis/hatchery_release_analysis/CVFRChin_RELEASE DB_v3_POSTED030419 (1).xlsx", sheet = "DBTable1") |> glimpse()
winter_releases <- readxl::read_excel("data-raw/analysis/hatchery_release_analysis/TEMP_ACollins_230424_98-21WCSRelease.xlsx", sheet = "Data") |> glimpse()

# notes - summarize by river
# have option for hatchery release in river OR at chipps
# baseline will be based off numbers currently released in river

# Fall quick summary
summarized_fall_release <- fall_releases |>
  group_by(Release_year, Release_location) |>
  summarise(total_release = sum(Total_N, na.rm = TRUE)) |>
  glimpse()

summarized_fall_release$Release_location |> unique()

summary(summarized_fall_release)
# Annual releases range from 1,010,176 - 56,462,517 (mean: 29,612,486)
# # TODO look for production targets

# winter quick summary
summarized_winter_release <- winter_releases |>
  mutate(total_released = `ReleasedClip/Tag` + `ReleasedNoClip/Tag` + `ReleasedClip/NoTag`) |>
  group_by(year = lubridate::year(InitialReleaseDate)) |>
  summarise(total_release = sum(total_released, na.rm = TRUE)) |>
  glimpse()
