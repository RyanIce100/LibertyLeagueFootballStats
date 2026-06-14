##2019 data scraping
stats_file  <- "2019_rocS.html"
roster_file <- "2019_rocR.html"

roc_stats_2019 <- scrape_rpi_football_stats(stats_file, roster_file)

roc_stats_2019 <- roc_stats_2019[-c(14, 64,51), ]

roc_stats_2019 <- roc_stats_2019 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))


## 2021 data scraping
stats_file  <- "2021_rocS.html"
roster_file <- "2021_rocR.html"

roc_stats_2021 <- scrape_rpi_football_stats(stats_file, roster_file)

roc_stats_2021 <- roc_stats_2021[-c(10,62), ]
roc_stats_2021 <- roc_stats_2021 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2022 data scraping
stats_file  <- "2022_rocS.html"
roster_file <- "2022_rocR.html"

roc_stats_2022 <- scrape_rpi_football_stats(stats_file, roster_file)

roc_stats_2022[13, "Yr"] <- "Freshman"
roc_stats_2022[13, "Pos"] <- "WR"


roc_stats_2022 <- roc_stats_2022[-c(9,49,51), ]
roc_stats_2022 <- roc_stats_2022 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2023 data scraping
stats_file  <- "2023_rocS.html"
roster_file <- "2023_rocR.html"

roc_stats_2023 <- scrape_rpi_football_stats(stats_file, roster_file)
roc_stats_2023[16, "Yr"] <- "Sophomore"
roc_stats_2023[16, "Pos"] <- "WR"
roc_stats_2023[38, "Yr"] <- "Freshman"
roc_stats_2023[38, "Pos"] <- "DB"

roc_stats_2023 <- roc_stats_2023[-c(9,55,56), ]

roc_stats_2023 <- roc_stats_2023 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2024 data scraping
stats_file  <- "2024_rocS.html"
roster_file <- "2024_rocR.html"

roc_stats_2024 <- scrape_rpi_football_stats(stats_file, roster_file)

roc_stats_2024[40, "Yr"] <- "Junior"
roc_stats_2024[40, "Pos"] <- "WR"
roc_stats_2024[37, "Yr"] <- "Sophomore"
roc_stats_2024[37, "Pos"] <- "DB"

roc_stats_2024 <- roc_stats_2024[-c(13), ]

roc_stats_2024 <- roc_stats_2024 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2025 data scraping
stats_file  <- "2025_rocS.html"
roster_file <- "2025_rocR.html"

roc_stats_2025 <- scrape_rpi_football_stats(stats_file, roster_file)

roc_stats_2025[13, "Yr"] <- "Senior"
roc_stats_2025[13, "Pos"] <- "WR"
roc_stats_2025[40, "Yr"] <- "Freshman"
roc_stats_2025[40, "Pos"] <- "DB"


roc_stats_2025 <- roc_stats_2025[-c(9), ]
roc_stats_2025 <- roc_stats_2025 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## combined data
roc_combined <- bind_rows(roc_stats_2019,roc_stats_2021,roc_stats_2022,roc_stats_2023,roc_stats_2024,roc_stats_2025)
write.csv(roc_combined, "roc_data.csv", row.names = FALSE)








