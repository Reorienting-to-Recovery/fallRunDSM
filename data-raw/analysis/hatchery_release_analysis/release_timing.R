library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
library(ggridges)
# devtools::install_github("dgrtwo/gganimate")
# library(gganimate)

fall_releases <- readxl::read_excel(path = "data-raw/analysis/hatchery_release_analysis/CVFRChin_RELEASE DB_v3_POSTED030419 (1).xlsx", sheet = "DBTable1") |> glimpse()
# winter_releases <- readxl::read_excel("data-raw/analysis/hatchery_release_analysis/TEMP_ACollins_230424_98-21WCSRelease.xlsx", sheet = "Data") |> glimpse()

# notes - summarize by river
# have option for hatchery release in river OR at chipps
# baseline will be based off numbers currently released in river

uncounted_releases <- fall_releases |>
  # filter(year(Avg_date) > 1980, year(Avg_date) < 2000) |>
  dplyr::transmute(date = Avg_date,
            year = year(date),
            hatchery = Hatchery,
            count_thousands = round(Total_N/1000, 0),
            average_fl_released = Avg_FL_mm) |>
  filter(!is.na(date))

uncounted_releases |>
  filter(year > 2000) |>
  ggplot(aes(x = date)) +
  geom_density() +
  theme_minimal() +
  facet_wrap(~year, scales = "free", ncol = 1)



daily_releases <- fall_releases |>
  # filter(year(Avg_date) > 1980, year(Avg_date) < 2000) |>
  dplyr::transmute(date = Avg_date,
            year = year(date),
            hatchery = Hatchery,
            count_thousands = round(Total_N/1000, 0),
            average_fl_released = Avg_FL_mm) |>
  filter(!is.na(date)) |>
  group_by(date) |>
  summarise(total_count_thousands = sum(count_thousands, na.rm = TRUE)) |>
  mutate(year = as.character((year(date))),
         fake_date = as_date(paste(1900, "-", month(date), day(date)))) |>
  filter(!is.na(fake_date)) |> glimpse()


ggplot(daily_releases, aes(x = fake_date, y = year)) + geom_density_ridges2()

library(scales)
ggplot(daily_releases,
                    aes(x = fake_date, y = year, group = year, # basic aesthetics
                        cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=5, size = 0.25, rel_min_height = 0.02, fill = "#D5D5D3") +
  theme(legend.position = "none") +
  scale_x_date(labels = date_format("%b")) +
  labs(title = "Hatchery release distribution over time",
       y = "",
       x = "Month")

## animation
gg_shares + transition_manual(date, cumulative = TRUE)
