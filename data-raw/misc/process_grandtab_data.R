gt <- readxl::read_xls(here::here("data-raw", "misc", "GrandTab.2024.05.20.xls"),
                       sheet = "GrandTab") |>
  glimpse()


gt <- matrix(nrow = 31, ncol = 30, dimnames = list(rownames(fallRunDSM::adult_seeds),
                                                       as.character(1:30)))
no_fr_spawn <- !as.logical(DSMhabitat::watershed_species_present[1:31, ]$fr *
                             DSMhabitat::watershed_species_present[1:31,]$spawn)

# get 5 year average for all non fr watersheds, add to column 1, the rest can be 0

adult_seeds_gt <- matrix(0, nrow = 31, ncol = 30)
adult_seed_values_gt <- DSMCalibrationData::mean_escapement_2013_2017 %>%
  bind_cols(no_fr_spawn = no_fr_spawn) %>%
  select(watershed, Fall, no_fr_spawn) %>%
  mutate(corrected_fall = case_when(
    no_fr_spawn ~ 0,
    is.na(Fall) | Fall < 10 ~ 12,
    TRUE ~ Fall)
  ) %>% pull(corrected_fall)

adult_seeds_gt[ , 1] <- adult_seed_values_gt

rownames(adult_seeds_gt) <- DSMhabitat::watershed_species_present$watershed_name[-32]



# Fall run ----------------------------------------------------------------

fall_run_1 <- readxl::read_xls(here::here("data-raw", "misc", "GrandTab.2024.05.20.xls"),
                             sheet = "GrandTab",
                             range = "C410:V483") |>
  filter(!is.na(RunYear))
fall_run_2 <- readxl::read_xls(here::here("data-raw", "misc", "GrandTab.2024.05.20.xls"),
                               sheet = "GrandTab",
                               range = "C485:T558") |>
  filter(!is.na(RunYear))
fall_run_3 <- readxl::read_xls(here::here("data-raw", "misc", "GrandTab.2024.05.20.xls"),
                               sheet = "GrandTab",
                               range = "C560:N634") |>
  filter(!is.na(RunYear))
fall_run_4 <- readxl::read_xls(here::here("data-raw", "misc", "GrandTab.2024.05.20.xls"),
                               sheet = "GrandTab",
                               range = "C635:L708") |>
  filter(!is.na(RunYear))

all_fall <- full_join(fall_run_1, fall_run_2, by = "RunYear") |>
  full_join(fall_run_3, by = "RunYear") |>
  full_join(fall_run_4, by = "RunYear") |>
  pivot_longer(AboveRBDD:SUM2,
               names_to = "source",
               values_to = "count") |>
  filter(!str_detect(source, "SUM"),
         !source %in% c("Other.x", "Other.y")) |>
  mutate(run_year = readr::parse_number(RunYear),
         watershed = case_when(source %in% c("AboveRBDD", "BelowRBDD", "RBDDtransColusa") ~ "Upper Sacramento River",
                               source %in% c("Nimbus", "American") ~ "American River",
                               source %in% c("Battle", "Coleman", "BattleAboveCNFH", "KESWtransCNFH") ~ "Battle Creek",
                               source %in% c("Merced", "MerHat") ~ "Merced River",
                               source %in% c("FeaHat", "FeaRiv") ~ "Feather River",
                               source %in% c("Bear.x", "Bear.y") ~ "Bear River", # TODO Bear River vs. Bear Creek?
                               source %in% c("Antelope", "Ash", "Butte", "China", "Clear", "Dry",
                                             "Dye", "Cottonwood", "Cow", "Coyote", "Deer", "Mill",
                                             "Inks", "Paynes", "Natomas", "Olney", "Salt", "Singer",
                                             "Stillwater", "Thomes", "Toomes") ~ paste0(source, " Creek"),
                               source == "BigChico" ~ "Big Chico Creek",
                               source %in% c("Cosumnes", "Yuba", "Tuolumne", "Stanislaus") ~ paste0(source, " River"),
                               source == "Stoney" ~ "Stony Creek",
                               TRUE ~ source),
         site = case_when(source == "MokRiv" ~ "Mokelumne In-River",
                          source == "MokHat" ~ "Mokelumne River Hatchery",
                          source == "MerHat" ~ "Merced River Hatchery",
                          source == "Coleman" ~ "Coleman National Fish Hatchery",
                          source == "FeaRiv" ~ "Feather In-River",
                          source == "FeaHat" ~ "Feather Hatchery",
                          TRUE ~ source))

no_fr_spawn <- !as.logical(DSMhabitat::watershed_species_present[1:31, ]$fr *
                             DSMhabitat::watershed_species_present[1:31,]$spawn)

names_to_fill <- rownames(fallRunDSM::adult_seeds)[!no_fr_spawn]
names_to_fill[!names_to_fill %in% all_fall$watershed] # missing Elder and Calaveras

# TODO call grandtab_2020
# TODO call earlier grandtab_2017


