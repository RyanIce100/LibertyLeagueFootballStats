# =============================================================================
# Execution
# =============================================================================
## make sure to change team in function to corresponding one in mergable
##Hilbert data scraping
stats_file  <- "hilS.html"
roster_file <- "hilR.html"

hil_stats <- scrape_rpi_football_stats(stats_file, roster_file)
hil_stats <- hil_stats[-c(18,57,58, 30, 38, 41,42,43,45,47,53,56,59), ]
hil_stats <- hil_stats |>
  mutate(Yr = case_when(
    Yr == "So." ~ "Sophomore",
    Yr == "Sr." ~ "Senior",
    Yr == "Fr." ~ "Freshman",
    Yr == "Jr." ~ "Junior",
    Yr == "5th" ~ "Graduate",
    Yr == "Gr." ~ "Graduate"
  ))
hil_stats$Season <- 2025
write.csv(hil_stats, "hil_stats.csv", row.names = FALSE)
