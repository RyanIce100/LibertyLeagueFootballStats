library(rvest)
library(dplyr)
library(stringr)
library(purrr)

##2019 data scraping
stats_file  <- "2019_hobS.html"
roster_file <- "2019_hobR.html"

hob_stats_2019 <- scrape_rpi_football_stats(stats_file, roster_file)
hob_stats_2019 <- hob_stats_2019 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    TRUE ~ Yr
  ))
hob_stats_2019[3, "Yr"] <- "Freshman"
hob_stats_2019[3, "Pos"] <- "RB"
hob_stats_2019 <- hob_stats_2019[-c(13,52), ]

##2021 data scraping
stats_file  <- "2021_hobS.html"
roster_file <- "2021_hobR.html"

hob_stats_2021 <- scrape_rpi_football_stats(stats_file, roster_file)
hob_stats_2021[1, "Yr"] <- "Junior"
hob_stats_2021[1, "Pos"] <- "RB"

hob_stats_2021 <- hob_stats_2021[-c(71,15,49), ]
hob_stats_2021 <- hob_stats_2021 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    TRUE ~ Yr
  ))

## 2022 data scraping
stats_file  <- "2022_hobS.html"
roster_file <- "2022_hobR.html"

hob_stats_2022 <- scrape_rpi_football_stats(stats_file, roster_file)
hob_stats_2022 <- hob_stats_2022[-c(36,89, 92), ]
hob_stats_2022 <- hob_stats_2022 |>
  mutate(Yr = case_when(
    Yr == "FY" ~ "Freshman",
    Yr == "SO" ~ "Sophomore",
    Yr == "SR" ~ "Senior",
    Yr == "JR" ~ "Junior",
    Yr == "GR" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2023 data scraping
stats_file  <- "2023_hobS.html"
roster_file <- "2023_hobR.html"

hob_stats_2023 <- scrape_rpi_football_stats(stats_file, roster_file)
hob_stats_2023[57, "Player"] <- "Bangalay-Stephen, Berte"
hob_stats_2023[57, "Yr"] <- "Junior"
hob_stats_2023[57, "Pos"] <- "DL"
hob_stats_2023 <- hob_stats_2023[-c(14,68), ]
hob_stats_2023 <- hob_stats_2023 |>
  mutate(Yr = case_when(
    Yr == "FY" ~ "Freshman",
    Yr == "SO" ~ "Sophomore",
    Yr == "SR" ~ "Senior",
    Yr == "JR" ~ "Junior",
    Yr == "GR" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2024 data scraping
stats_file  <- "2024_hobS.html"
roster_file <- "2024_hobR.html"

hob_stats_2024 <- scrape_rpi_football_stats(stats_file, roster_file)
hob_stats_2024 <- hob_stats_2024[-c(11,61), ]
hob_stats_2024 <- hob_stats_2024 |>
  mutate(Yr = case_when(
    Yr == "FY" ~ "Freshman",
    Yr == "SO" ~ "Sophomore",
    Yr == "SR" ~ "Senior",
    Yr == "JR" ~ "Junior",
    Yr == "GR" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2025 data scraping
stats_file  <- "2025_hobS.html"
roster_file <- "2025_hobR.html"

hob_stats_2025 <- scrape_rpi_football_stats(stats_file, roster_file)
hob_stats_2025[24, "Yr"] <- "Sophomore"
hob_stats_2025[24, "Pos"] <- "WR"

hob_stats_2025 <- hob_stats_2025[-c(44,47,61,15,63,65), ]
hob_stats_2025 <- hob_stats_2025 |>
  mutate(Yr = case_when(
    Yr == "FY" ~ "Freshman",
    Yr == "SO" ~ "Sophomore",
    Yr == "SR" ~ "Senior",
    Yr == "JR" ~ "Junior",
    Yr == "GR" ~ "Graduate",
    TRUE ~ Yr
  ))

## combining data
hob_combined <- bind_rows(hob_stats_2019,hob_stats_2021,hob_stats_2022,hob_stats_2023,hob_stats_2024,hob_stats_2025)
write.csv(hob_combined, "hob_data.csv", row.names = FALSE)







