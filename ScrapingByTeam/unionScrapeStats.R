##2019 data scraping
stats_file  <- "2019_uniS.html"
roster_file <- "2019_uniR.html"

uni_stats_2019 <- scrape_rpi_football_stats(stats_file, roster_file)
uni_stats_2019 <- uni_stats_2019[-c(15,16,17,13,20,21,22,71,73,75,77), ]
uni_stats_2019 <- uni_stats_2019 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))
#13,15,30,40,61,62
uni_stats_2019[13, "Yr"] <- "Sophomore"
uni_stats_2019[13, "Pos"] <- "WR"
uni_stats_2019[15, "Yr"] <- "Junior"
uni_stats_2019[15, "Pos"] <- "WR"
uni_stats_2019[30, "Player"] <- "St. Pierre, Austin"
uni_stats_2019[30, "Yr"] <- "Sophomore"
uni_stats_2019[30, "Pos"] <- "S"
uni_stats_2019[40, "Yr"] <- "Freshman"
uni_stats_2019[40, "Pos"] <- "LB"
uni_stats_2019[61, "Yr"] <- "Senior"
uni_stats_2019[61, "Pos"] <- "K"
uni_stats_2019[62, "Yr"] <- "Freshman"
uni_stats_2019[62, "Pos"] <- "OL"

## 2021 data scraping
stats_file  <- "2021_uniS.html"
roster_file <- "2021_uniR.html"

uni_stats_2021 <- scrape_rpi_football_stats(stats_file, roster_file)
uni_stats_2021 <- uni_stats_2021[-c(9,71,13,14,15), ]
uni_stats_2021[7, "Yr"] <- "Senior"
uni_stats_2021[7, "Pos"] <- "S"
uni_stats_2021[11, "Yr"] <- "Senior"
uni_stats_2021[11, "Pos"] <- "WR"
uni_stats_2021[14, "Yr"] <- "Senior"
uni_stats_2021[14, "Pos"] <- "WR"
uni_stats_2021[22, "Yr"] <- "Junior"
uni_stats_2021[22, "Pos"] <- "LB"
uni_stats_2021[40, "Player"] <- "Goldstein, Spencer"
uni_stats_2021[40, "Yr"] <- "Junior"
uni_stats_2021[40, "Pos"] <- "LB"
uni_stats_2021[63, "Yr"] <- "Junior"
uni_stats_2021[63, "Pos"] <- "OL"
uni_stats_2021 <- uni_stats_2021 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))

## 2022 data scraping
stats_file  <- "2022_uniS.html"
roster_file <- "2022_uniR.html"

uni_stats_2022 <- scrape_rpi_football_stats(stats_file, roster_file)
uni_stats_2022 <- uni_stats_2022[-c(13,68), ]
uni_stats_2022[15, "Yr"] <- "Senior"
uni_stats_2022[15, "Pos"] <- "WR"
uni_stats_2022[29, "Yr"] <- "Senior"
uni_stats_2022[29, "Pos"] <- "S"
uni_stats_2022 <- uni_stats_2022[-54, ]
uni_stats_2022 <- uni_stats_2022 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))

## 2023 data scraping
stats_file  <- "2023_uniS.html"
roster_file <- "2023_uniR.html"

uni_stats_2023 <- scrape_rpi_football_stats(stats_file, roster_file)
uni_stats_2023 <- uni_stats_2023[-c(14,71,72), ]
uni_stats_2023[56, "Yr"] <- "Freshman"
uni_stats_2023[56, "Pos"] <- "DB"
uni_stats_2023 <- uni_stats_2023[-6, ]
uni_stats_2023 <- uni_stats_2023 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))

## 2024 data scraping
stats_file  <- "2024_uniS.html"
roster_file <- "2024_uniR.html"

uni_stats_2024 <- scrape_rpi_football_stats(stats_file, roster_file)
uni_stats_2024 <- uni_stats_2024[-c(11,15,64,66), ]
uni_stats_2024[18, "Yr"] <- "Freshman"
uni_stats_2024[18, "Pos"] <- "WR"
uni_stats_2024[41, "Player"] <- "Johnson, Charlie"
uni_stats_2024[41, "Yr"] <- "Junior"
uni_stats_2024[41, "Pos"] <- "DL"
uni_stats_2024[51, "Player"] <- "Barnes-Pace, Michael"
uni_stats_2024[51, "Yr"] <- "Freshman"
uni_stats_2024[51, "Pos"] <- "DL"
uni_stats_2024[58, "Yr"] <- "Sophomore"
uni_stats_2024[58, "Pos"] <- "WR"
uni_stats_2024 <- uni_stats_2024 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))

## 2025 data scraping
stats_file  <- "2025_uniS.html"
roster_file <- "2025_uniR.html"

uni_stats_2025 <- scrape_rpi_football_stats(stats_file, roster_file)
uni_stats_2025 <- uni_stats_2025[-c(15,72), ]
uni_stats_2025[44, "Yr"] <- "Freshman"
uni_stats_2025[44, "Pos"] <- "DB"
uni_stats_2025 <- uni_stats_2025 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))

## combined data
uni_combined <- bind_rows(uni_stats_2019,uni_stats_2021,uni_stats_2022, uni_stats_2023, uni_stats_2024, uni_stats_2025)
write.csv(uni_combined, "uni_data.csv", row.names = FALSE)























