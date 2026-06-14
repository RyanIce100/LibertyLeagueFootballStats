library(rvest)
# =============================================================================
# Execution
# =============================================================================
##2019 data scraping
rpi_stats_2019 <- scrape_rpi_football_stats(stats_file, roster_file)
rpi_stats_2019 <- rpi_stats_2019 |>
  mutate(Yr = case_when(
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    Yr == "Jr." ~ "Junior"
  ))
rpi_stats_2019 <- rpi_stats_2019 |>
  mutate(Yr = case_when(
    Player == "Guiasola, Andres" ~ "Senior",
    Player == "Brockdroff, S" ~ "Freshman",
    TRUE ~ Yr
  ))
rpi_stats_2019 <- rpi_stats_2019 |>
  mutate(Pos = case_when(
    Player == "Guiasola, Andres" ~ "DB",
    Player == "Brockdroff, S" ~ "DB",
    TRUE ~ Pos
  ))
rpi_stats_2019 <- rpi_stats_2019 |>
  mutate(Player = case_when(
    Player == "Brockdroff, S" ~ "Brockdorff, Spencer",
    TRUE ~ Player
  ))
rpi_stats_2019 <- rpi_stats_2019[-22, ]
  
##2021 data scraping
stats_file  <- "2021_rpiS.html"
roster_file <- "2021_rpi.html"
rpi_stats_2021 <- scrape_rpi_football_stats(stats_file, roster_file)
rpi_stats_2021 <- rpi_stats_2021[-c(3:9), ]
rpi_stats_2021 <- rpi_stats_2021 |>
  mutate(Yr = case_when(
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    Yr == "Jr." ~ "Junior",
    Yr == "5th" ~ "Graduate",
    Yr == "Gr." ~ "Graduate"
  ))
rpi_stats_2021 <- rpi_stats_2021[-c(66,58,19), ]
rpi_stats_2021 <- rpi_stats_2021[-41, ]
rpi_stats_2021 <- rpi_stats_2021[-27, ]
rpi_stats_2021 <- rpi_stats_2021[-c(12, 26, 75), ]
rpi_stats_2021 <- rpi_stats_2021[-c(70,62, 47), ]

##2022 data scraping
stats_file  <- "2022_rpiS.html"
roster_file <- "2022_rpiR.html"
rpi_stats_2022 <- scrape_rpi_football_stats(stats_file, roster_file)
rpi_stats_2022 <- rpi_stats_2022 |>
  mutate(Yr = case_when(
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    Yr == "Jr." ~ "Junior",
    Yr == "5th" ~ "Graduate",
    Yr == "Gr." ~ "Graduate"
  ))
rpi_stats_2022 <- rpi_stats_2022[-c(13,62, 66, 64), ]
rpi_stats_2022[63, "FGA.20.29"] <- 4
rpi_stats_2022[63, "FGM.20.29"] <- 7
rpi_stats_2022[7, "Player"] <- "Shuster, Daniel"
rpi_stats_2022[7, "Pos"] <- "QB"
rpi_stats_2022[7, "Yr"] <- "Freshman"

##2023 data scraping
stats_file  <- "2023_rpiS.html"
roster_file <- "2023_rpiR.html"
rpi_stats_2023 <- scrape_rpi_football_stats(stats_file, roster_file)
rpi_stats_2023 <- rpi_stats_2023 |>
  mutate(Yr = case_when(
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    Yr == "Jr." ~ "Junior",
    Yr == "5th" ~ "Graduate",
    Yr == "Gr." ~ "Graduate"
  ))
rpi_stats_2023 <- rpi_stats_2023[-c(25, 91, 92), ]
rpi_stats_2023[31, "Player"] <- "LaFrance, Zach"
rpi_stats_2023[31, "Pos"] <- "WR"
rpi_stats_2023[31, "Yr"] <- "Sophomore"
rpi_stats_2023[91, "Player"] <- "Gagliardi, Nick"
rpi_stats_2023[91, "Pos"] <- "TE"
rpi_stats_2023[91, "Yr"] <- "Senior"
rpi_stats_2023[81, "Player"] <- "O'Leary, Alex"
rpi_stats_2023[81, "Pos"] <- "Ls"
rpi_stats_2023[81, "Yr"] <- "Freshman"
rpi_stats_2023[86, "Player"] <- "Kwak, JiMin"
rpi_stats_2023[86, "Pos"] <- "DL"
rpi_stats_2023[86, "Yr"] <- "Freshman"

##2024 data scraping
stats_file  <- "2024_rpiS.html"
roster_file <- "2024_rpiR.html"
rpi_stats_2024 <- scrape_rpi_football_stats(stats_file, roster_file)
rpi_stats_2024 <- rpi_stats_2024 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    TRUE ~ Yr
  ))
rpi_stats_2024 <- rpi_stats_2024[-c(18,95,4,13,14,16,19,20,22,29,30,43,53,54,90,91,92,93,94), ]
rpi_stats_2024 <- rpi_stats_2024[-c(40,41,37), ]
rpi_stats_2024[8, "Pos"] <- "QB"
rpi_stats_2024[8, "Yr"] <- "Junior"
rpi_stats_2024[8, "Player"] <- "Connolly, Luke"
rpi_stats_2024[64, "Pos"] <- "DB"
rpi_stats_2024[64, "Yr"] <- "Freshman"
rpi_stats_2024[37, "Pos"] <- "LB"
rpi_stats_2024[37, "Yr"] <- "Senior"
rpi_stats_2024 <- rpi_stats_2024[-c(61,58), ]
rpi_stats_2024 <- rpi_stats_2024 |>
  mutate(Pos = case_when(
    Pos == "Quarterback" ~ "QB",
    Pos == "Running Back" ~ "RB",
    Pos == "Linebacker" ~ "LB",
    Pos == "Defensive Back" ~ "DB",
    Pos == "Defensive Line" ~ "DL",
    Pos == "Wide Receiver" ~ "WR",
    Pos == "Tight End" ~ "TE",
    Pos == "Offensive Line" ~ "OL",
    Pos == "Slot Receiver" ~ "SR",
    Pos == "Kicker / Punter" ~ "K/P",
    Pos == "Punter / Kicker" ~ "P/K",
    TRUE ~ Pos
  ))

## 2025 data Scraping
stats_file  <- "2025_rpiS.html"
roster_file <- "2025_rpiR.html"
rpi_stats_2025 <- scrape_rpi_football_stats(stats_file, roster_file)
rpi_stats_2025 <- rpi_stats_2025 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    TRUE ~ Yr
  ))
rpi_stats_2025[70, "Pos"] <- "QB"
rpi_stats_2025[70, "Yr"] <- "Freshman"
rpi_stats_2025[70, "Player"] <- "Riccitelli, Christian"
rpi_stats_2025 <- rpi_stats_2025[-c(16,62,74, 75), ]
rpi_stats_2025[15, "Pos"] <- "RB"
rpi_stats_2025[15, "Yr"] <- "Senior"
rpi_stats_2025 <- rpi_stats_2025 |>
  mutate(Pos = case_when(
    Pos == "Quarterback" ~ "QB",
    Pos == "Running Back" ~ "RB",
    Pos == "Linebacker" ~ "LB",
    Pos == "Defensive Back" ~ "DB",
    Pos == "Defensive Line" ~ "DL",
    Pos == "Wide Receiver" ~ "WR",
    Pos == "Tight End" ~ "TE",
    Pos == "Offensive Line" ~ "OL",
    Pos == "Slot Receiver" ~ "SR",
    Pos == "Kicker / Punter" ~ "K/P",
    Pos == "Punter / Kicker" ~ "P/K",
    Pos == "Long Snapper" ~ "LS",
    TRUE ~ Pos
  ))

##append datasets together:
rpi_combined <- bind_rows(rpi_stats_2019, rpi_stats_2021, rpi_stats_2022, rpi_stats_2023, rpi_stats_2024, rpi_stats_2025)
rpi_combined <- rpi_combined[-208, ]
write.csv(rpi_combined, "rpi_data.csv", row.names = FALSE)





