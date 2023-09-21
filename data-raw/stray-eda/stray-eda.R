library(tidyverse)

all_data <- read_csv("data-raw/stray-eda/wetransfer_sturrock-et-al/input_data/alldata_formodel_031918.csv")
all_data |> pull(Cleaned.up.site.name) |> table() |> sort(decreasing = TRUE)

all_data |>
  group_by(Bay) |>
  tally()

bay_released <- all_data |> filter(Bay == "BAY")
non_bay_released <- all_data |> filter(Bay != "BAY")


