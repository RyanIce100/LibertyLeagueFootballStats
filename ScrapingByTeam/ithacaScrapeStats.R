library(rvest)
# =============================================================================
# Execution
# =============================================================================
## make sure to change team in function to corresponding one in mergable
##2019 data scraping
stats_file  <- "2019_ithS.html"
roster_file <- "2019_ithR.html"

ith_stats_2019 <- scrape_rpi_football_stats(stats_file, roster_file)

ith_stats_2019[44, "Yr"] <- "Sophomore"
ith_stats_2019[44, "Player"] <- "Watson II, Warren"
ith_stats_2019[44, "Pos"] <- "DL"
ith_stats_2019[52, "Yr"] <- "Sophomore"
ith_stats_2019[52, "Player"] <- "DeSimpliciis, Matt"
ith_stats_2019[52, "Pos"] <- "LB"
ith_stats_2019[56, "Yr"] <- "Freshman"
ith_stats_2019[56, "Player"] <- "Kaiser, Ryan"
ith_stats_2019[56, "Pos"] <- "LB"
ith_stats_2019[58, "Yr"] <- "Freshman"
ith_stats_2019[58, "Player"] <- "Soravilla, Stephen"
ith_stats_2019[58, "Pos"] <- "DB"
ith_stats_2019 <- ith_stats_2019[-13, ]
ith_stats_2019[21, "Pos"] <- "WR"
ith_stats_2019[6, "Pos"] <- "WR/QB"

ith_stats_2019 <- ith_stats_2019 |>
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
    Pos == "Defensive Lineman" ~ "DL",
    Pos == "Kicker" ~ "K",
    TRUE ~ Pos
  ))

## scrape data 2021
stats_file  <- "2021_ithS.html"
roster_file <- "2021_ithR.html"

ith_stats_2021 <- scrape_rpi_football_stats(stats_file, roster_file)
ith_stats_2021 <- ith_stats_2021[-c(28,77,80), ]
ith_stats_2021 <- ith_stats_2021 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))
ith_stats_2021 <- ith_stats_2021 |>
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
    Pos == "Defensive Lineman" ~ "DL",
    Pos == "Kicker" ~ "K",
    Pos == "Defensive Tackle" ~ "DT",
    TRUE ~ Pos
  ))

## 2022 scrape data
stats_file  <- "2022_ithS.html"
roster_file <- "2022_ithR.html"

ith_stats_2022 <- scrape_rpi_football_stats(stats_file, roster_file)
ith_stats_2022[54, "Player"] <- "Watson II, Warren"
ith_stats_2022[54, "Pos"] <- "DL"
ith_stats_2022[54, "Yr"] <- "Senior"
ith_stats_2022[53, "Player"] <- "Pusateri, Mike"
ith_stats_2022[53, "Pos"] <- "DL"
ith_stats_2022[53, "Yr"] <- "Junior"

ith_stats_2022 <- ith_stats_2022[-c(67,69,15), ]
ith_stats_2022 <- ith_stats_2022 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))
ith_stats_2022 <- ith_stats_2022 |>
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
    Pos == "Defensive Lineman" ~ "DL",
    Pos == "Kicker" ~ "K",
    Pos == "Defensive Tackle" ~ "DT",
    Pos == "Punter" ~ "P",
    TRUE ~ Pos
  ))

## scrape 2023 data
stats_file  <- "2023_ithS.html"
roster_file <- "2023_ithR.html"

ith_stats_2023 <- scrape_rpi_football_stats(stats_file, roster_file)
ith_stats_2023[52, "Player"] <- "Peters, Grant"
ith_stats_2023[52, "Pos"] <- "DL"
ith_stats_2023[52, "Yr"] <- "Junior"
ith_stats_2023[55, "Player"] <- "Capodilupo, Nick"
ith_stats_2023[55, "Pos"] <- "OL"
ith_stats_2023[55, "Yr"] <- "Senior"

ith_stats_2023 <- ith_stats_2023[-c(19,50), ]
ith_stats_2023 <- ith_stats_2023 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))
ith_stats_2023 <- ith_stats_2023 |>
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
    Pos == "Defensive Lineman" ~ "DL",
    Pos == "Kicker" ~ "K",
    Pos == "Defensive Tackle" ~ "DT",
    Pos == "Punter" ~ "P",
    TRUE ~ Pos
  ))

## 2024 scrape data
stats_file  <- "2024_ithS.html"
roster_file <- "2024_ithR.html"

ith_stats_2024 <- scrape_rpi_football_stats(stats_file, roster_file)

ith_stats_2024[52, "Player"] <- "Peters, Grant"
ith_stats_2024[52, "Pos"] <- "DL"
ith_stats_2024[52, "Yr"] <- "Senior"

ith_stats_2024 <- ith_stats_2024[-c(9,54, 50), ]
ith_stats_2024 <- ith_stats_2024 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))
ith_stats_2024 <- ith_stats_2024 |>
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
    Pos == "Defensive Lineman" ~ "DL",
    Pos == "Kicker" ~ "K",
    Pos == "Defensive Tackle" ~ "DT",
    Pos == "Punter" ~ "P",
    TRUE ~ Pos
  ))
ith_stats_2024[48, "Pos"] <- "DL"
ith_stats_2024[45, "Pos"] <- "TE/LS"

## 2025 data scraping
stats_file  <- "2025_ithS.html"
roster_file <- "2025_ithR.html"

ith_stats_2025 <- scrape_rpi_football_stats(stats_file, roster_file)
ith_stats_2025 <- ith_stats_2025[-c(17,59, 61), ]
ith_stats_2025 <- ith_stats_2025 |>
  mutate(Yr = case_when(
    Yr == "Graduate Student" ~ "Graduate",
    Yr == "First Year" ~ "Freshman",
    TRUE ~ Yr
  ))
ith_stats_2025 <- ith_stats_2025 |>
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
    Pos == "Defensive Lineman" ~ "DL",
    Pos == "Kicker" ~ "K",
    Pos == "Defensive Tackle" ~ "DT",
    Pos == "Punter" ~ "P",
    TRUE ~ Pos
  ))
ith_stats_2025[39, "Pos"] <- "DL"
ith_stats_2025[58, "Pos"] <- "K/P"
ith_stats_2025[57, "Pos"] <- "K/P/RB"
ith_stats_2025[25, "Pos"] <- "LS/TE"
ith_stats_2025[52, "Pos"] <- "RB/DB"

##append datasets together:
ith_combined <- bind_rows(ith_stats_2019,ith_stats_2021, ith_stats_2022, ith_stats_2023, ith_stats_2024, ith_stats_2025)
write.csv(ith_combined, "ith_data.csv", row.names = FALSE)







