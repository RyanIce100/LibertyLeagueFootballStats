##2019 data scraping
stats_file  <- "2019_bstS.html"
roster_file <- "2019_bstR.html"

bst_stats_2019 <- scrape_rpi_football_stats(stats_file, roster_file)

bst_stats_2019 <- bst_stats_2019[-c(7, 63,29,39,46), ]

bst_stats_2019[24, "Player"] <- "Wilson, Jay"
bst_stats_2019[24, "Pos"] <- "LB"
bst_stats_2019[24, "Yr"] <- "Junior"
bst_stats_2019[35, "Player"] <- "Christopher, Jaime"
bst_stats_2019[35, "Yr"] <- "Sophomore"
bst_stats_2019[35, "Pos"] <- "DL"

bst_stats_2019 <- bst_stats_2019[-c(14,16,30,42,43,46), ]


## 2021 data scraping
stats_file  <- "2021_bstS.html"
roster_file <- "2021_bstR.html"

bst_stats_2021 <- scrape_rpi_football_stats(stats_file, roster_file)

bst_stats_2021 <- bst_stats_2021[-c(7,71,72), ]
bst_stats_2021[31, "Pos"] <- "LB"
bst_stats_2021[31, "Yr"] <- "Junior"
bst_stats_2021[41, "Player"] <- "Ferguson, Dominic"
bst_stats_2021[41, "Yr"] <- "Sophomore"
bst_stats_2021[41, "Pos"] <- "LB"
bst_stats_2021[56, "Player"] <- "Alexis, Marven"
bst_stats_2021[56, "Yr"] <- "Freshman"
bst_stats_2021[56, "Pos"] <- "LB"
bst_stats_2021[62, "Player"] <- "Maeweather, Jeremiah"
bst_stats_2021[62, "Yr"] <- "Freshman"
bst_stats_2021[62, "Pos"] <- "DB"

bst_stats_2021 <- bst_stats_2021[-c(39,49,50,51,53,54,58,61,63,67), ]
bst_stats_2021[47, "Pos"] <- "Unknown"

bst_stats_2021 <- bst_stats_2021 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    Yr == "Fifth Year" ~ "Graduate",
    TRUE ~ Yr
  ))
bst_stats_2021 <- bst_stats_2021[-c(44,42), ]

## 2022 data scraping
stats_file  <- "2022_bstS.html"
roster_file <- "2022_bstR.html"

bst_stats_2022 <- scrape_rpi_football_stats(stats_file, roster_file)
bst_stats_2022 <- bst_stats_2022[-c(5,9,57,11,21,50), ]

bst_stats_2022 <- bst_stats_2022 |>
  mutate(Yr = case_when(
    Yr == "Jr." ~ "Junior",
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    TRUE ~ Yr
  ))
bst_stats_2022 <- bst_stats_2022[-c(33,42), ]

## 2023 data scraping
stats_file  <- "2023_bstS.html"
roster_file <- "2023_bstR.html"

bst_stats_2023 <- scrape_rpi_football_stats(stats_file, roster_file)
bst_stats_2023 <- bst_stats_2023[-c(12,62,58), ]

bst_stats_2023[46, "Player"] <- "O'Neill, Niah"
bst_stats_2023[46, "Yr"] <- "Freshman"
bst_stats_2023[46, "Pos"] <- "DB"

bst_stats_2023 <- bst_stats_2023[-42, ]
bst_stats_2023[30, "Pos"] <- "Unknown"

bst_stats_2023 <- bst_stats_2023 |>
  mutate(Yr = case_when(
    Yr == "Jr." ~ "Junior",
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    TRUE ~ Yr
  ))
bst_stats_2023 <- bst_stats_2023[-42, ]

## 2024 data scraping
stats_file  <- "2024_bstS.html"
roster_file <- "2024_bstR.html"

bst_stats_2024 <- scrape_rpi_football_stats(stats_file, roster_file)
bst_stats_2024 <- bst_stats_2024[-c(12,63,65,21,43), ]

bst_stats_2024[14, "Yr"] <- "Senior"
bst_stats_2024[14, "Pos"] <- "WR"

bst_stats_2024 <- bst_stats_2024[-c(45,55), ]
bst_stats_2024 <- bst_stats_2024[-41, ]

bst_stats_2024 <- bst_stats_2024 |>
  mutate(Yr = case_when(
    Yr == "Jr." ~ "Junior",
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    TRUE ~ Yr
  ))

## 2025 data scraping
stats_file  <- "2025_bstS.html"
roster_file <- "2025_bstR.html"

bst_stats_2025 <- scrape_rpi_football_stats(stats_file, roster_file)
bst_stats_2025 <- bst_stats_2025[-c(20,21,22,9,10,48,53,63,64,65,68), ]
bst_stats_2025[54, "Player"] <- "Masetta, Steven"
bst_stats_2025[54, "Yr"] <- "Freshman"
bst_stats_2025[54, "Pos"] <- "WR/K"
bst_stats_2025[55, "Player"] <- "Severin, Ptah"
bst_stats_2025[55, "Yr"] <- "Freshman"
bst_stats_2025[55, "Pos"] <- "QB"

bst_stats_2025 <- bst_stats_2025[-c(36,50,51,42,27,58), ]

bst_stats_2025 <- bst_stats_2025 |>
  mutate(Yr = case_when(
    Yr == "Jr." ~ "Junior",
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    TRUE ~ Yr
  ))

## combined data
bst_combined <- bind_rows(bst_stats_2019,bst_stats_2021,bst_stats_2022,bst_stats_2023,bst_stats_2024,bst_stats_2025)
write.csv(bst_combined, "bst_data.csv", row.names = FALSE)








