library(tidyverse)


fall_releases <- readxl::read_excel(path = "data-raw/analysis/hatchery_release_analysis/CVFRChin_RELEASE DB_v3_POSTED030419 (1).xlsx", sheet = "DBTable1") |> glimpse()
winter_releases <- readxl::read_excel("data-raw/analysis/hatchery_release_analysis/TEMP_ACollins_230424_98-21WCSRelease.xlsx", sheet = "Data") |> glimpse()

# Fall quick summary
summarized_fall_release <- fall_releases |>
  group_by(Release_year) |>
  summarise(total_release = sum(Total_N, na.rm = TRUE)) |>
  glimpse()

summary(summarized_fall_release)
# Annual releases range from 1,010,176 - 56,462,517 (mean: 29,612,486)

# winter quick summary
summarized_winter_release <- winter_releases |>
  mutate(total_released = `ReleasedClip/Tag` + `ReleasedNoClip/Tag` + `ReleasedClip/NoTag`) |>
  group_by(year = lubridate::year(InitialReleaseDate)) |>
  summarise(total_release = sum(total_released, na.rm = TRUE)) |>
  glimpse()
