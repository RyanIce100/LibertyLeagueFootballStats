##2019 data scraping
stats_file  <- "2019_sluS.html"
roster_file <- "2019_sluR.html"

slu_stats_2019 <- scrape_rpi_football_stats(stats_file, roster_file)
slu_stats_2019 <- slu_stats_2019[-c(8, 49,59), ]
slu_stats_2019 <- slu_stats_2019 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    TRUE ~ Yr
  ))
slu_stats_2019[44, "Player"] <- "Goldstien, Maison"
slu_stats_2019[44, "Yr"] <- "Sophomore"
slu_stats_2019[44, "Pos"] <-"DB"
  

## 2021 data scraping
stats_file  <- "2021_sluS.html"
roster_file <- "2021_sluR.html"

slu_stats_2021 <- scrape_rpi_football_stats(stats_file, roster_file)
slu_stats_2021[47, "Player"] <- "Goldstien, Maison"
slu_stats_2021[47, "Yr"] <- "Senior"
slu_stats_2021[47, "Pos"] <-"DB"
slu_stats_2021[69, "Player"] <- "Giunta, Mason"
slu_stats_2021[69, "Yr"] <- "Senior"
slu_stats_2021[69, "Pos"] <-"RB"


slu_stats_2021 <- slu_stats_2021[-c(15, 52, 46, 60, 39), ]
slu_stats_2021 <- slu_stats_2021 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2022 data scraping
stats_file  <- "2022_sluS.html"
roster_file <- "2022_sluR.html"

slu_stats_2022 <- scrape_rpi_football_stats(stats_file, roster_file)
slu_stats_2022 <- slu_stats_2022[-c(46, 7), ]
slu_stats_2022 <- slu_stats_2022 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2023 data scraping
stats_file  <- "2023_sluS.html"
roster_file <- "2023_sluR.html"

slu_stats_2023 <- scrape_rpi_football_stats(stats_file, roster_file)
slu_stats_2023 <- slu_stats_2023[-c(16, 62,63,51,24,50,59), ]
slu_stats_2023 <- slu_stats_2023 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2024 data scraping
stats_file  <- "2024_sluS.html"
roster_file <- "2024_sluR.html"

slu_stats_2024 <- scrape_rpi_football_stats(stats_file, roster_file)
slu_stats_2024 <- slu_stats_2024[-c(17,58,59), ]
slu_stats_2024 <- slu_stats_2024 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## 2025 data scraping
stats_file  <- "2025_sluS.html"
roster_file <- "2025_sluR.html"

slu_stats_2025 <- scrape_rpi_football_stats(stats_file, roster_file)

slu_stats_2025 <- slu_stats_2025[-c(11,17,61,26,31,34,71,69), ]
slu_stats_2025 <- slu_stats_2025 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First-Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))

## combined data
slu_combined <- bind_rows(slu_stats_2019,slu_stats_2021,slu_stats_2022,slu_stats_2023,slu_stats_2024,slu_stats_2025)
write.csv(slu_combined, "slu_data.csv", row.names = FALSE)








