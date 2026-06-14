library(ggthemes)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

dIIIData <- read.csv("DIII Player Stats.csv", fileEncoding = "latin1")
LLData <- dIIIData |>
  filter(Team %in% c("Rensselaer", "Ithaca", "Hobart","Rochester_NY","Union_NY","St_Lawrence","Springfield","WPI","Merchant_Marine" ))
LLData_clean <- LLData |>
  filter(!(Team %in% c("Springfield","WPI","Merchant_Marine") & Season > 2016))
LLData_clean <- LLData_clean |>
  filter(!(Team == "Ithaca" & Season <= 2016))

#Standardize data with scraped data
LLData_clean_mergable <- LLData_clean |>
  select(-c(X, Fum.Ret, Fum.Ret.Yds, Fum.Ret.TD, Int.Ret.TD, Kick.Ret.PAT,Fum.Ret.PAT, Fum.Lost, FGA.60.Plus, FGM.60.Plus, GS, Pass.2Pt.Att, Pass.2Pt.Conv, Rush.2Pt.Att, Rush.2Pt.Conv, Rec.2Pt.Conv, PAT.Att, PAT.Made))
LLData_clean_mergable$Solo.Sack = LLData_clean_mergable$Solo.Sack + LLData_clean_mergable$Asst.Sack
LLData_clean_mergable$Solo.TFL = LLData_clean_mergable$Solo.TFL + LLData_clean_mergable$Asst.TFL
LLData_clean_mergable <- LLData_clean_mergable |>
  select(-c(Asst.Sack,Asst.TFL))
names(LLData_clean_mergable)[names(LLData_clean_mergable) == 'Solo.Sack'] <- 'Sack'
names(LLData_clean_mergable)[names(LLData_clean_mergable) == 'Solo.TFL'] <- 'TFL'

# Sample data analysis
# LLDataRecCombined <- LLData_clean |>
#   group_by(Player, Team) |>
#   summarize(
#     Rec_total = sum(Rec, na.rm = TRUE),
#     Rec.Yds_total = sum(Rec.Yds, na.rm = TRUE),
#     Rec.TD_total = sum(Rec.TD, na.rm = TRUE),
#     Yds_per_Rec = Rec.Yds_total / Rec_total, 
#     games_played = sum(GP, na.rm = TRUE),
#     rec_yds_pg = Rec.Yds_total/games_played,
#     rec_tds_pg = Rec.TD_total / games_played,
#     first_season = min(Season, na.rm = TRUE),
#   ) |>
#   filter(is.finite(Yds_per_Rec)) |>
#   ungroup()
# 
# LLDataRecFiltered <- LLDataRecCombined |>
#   filter(Rec_total > 50)
# 
# color_count_team <- length(unique(LLDataRushFiltered$Team))
# get_palette <- colorRampPalette(brewer.pal(9,"Set1"))
# 
# 
# ggplot(LLDataRecFiltered, aes(x=rec_yds_pg, y = rec_tds_pg, color = Team, label = Player)) +
#   geom_point() +
#   theme_minimal() +
#   geom_text_repel() +
#   xlab("Receiving Yards Per Game") +
#   ylab("Receiving Touchdowns Per Game") +
#   ggtitle("Receiving Touchdowns Per Game vs Receiving Yards Per Game", subtitle = "    Among LL receivers with 50+ Rec. between 2013-2018") +
#   scale_color_manual("Team", 
#                      values = get_palette(color_count_team))
# 
# LLDataPassCombined <- LLData_clean |>
#   group_by(Player, Team) |>
#   summarize(
#     Pass.Att_total = sum(Pass.Att, na.rm = TRUE),
#     Pass.Yds_total = sum(Pass.Yds),
#     ypa = Pass.Yds_total / Pass.Att_total,
#     Pass.TD_total = sum(Pass.TD, na.rm = TRUE),
#     Int.Thrown_total = sum(Int.Thrown, na.rm = TRUE),
#     games_played = sum(GP, na.rm = TRUE),
#     pass_yds_pg = Pass.Yds_total/games_played,
#     pass_tds_pg = Pass.TD_total / games_played,
#     pass_int_pg = Int.Thrown_total / games_played,
#     first_season = min(Season, na.rm = TRUE),
#   ) |>
#   filter(is.finite(ypa)) |>
#   ungroup()
# 
# LLDataPassFiltered <- LLDataPassCombined |>
#   filter(Pass.Att_total > 100)
# 
# 
# ggplot(LLDataPassFiltered, aes(x=pass_yds_pg, y = pass_tds_pg, color = Team, label = Player)) +
#   geom_point() +
#   theme_minimal() +
#   geom_text_repel() +
#   geom_vline(xintercept = mean(LLDataPassFiltered$pass_yds_pg), linetype = "dashed", color = "black") +
#   geom_hline(yintercept = mean(LLDataPassFiltered$pass_tds_pg), linetype = "dashed", color = "black") +
#   xlab("Passing Yards Per Game") +
#   ylab("Passing Touchdowns Per Game") +
#   ggtitle("Passing Yards Per Game vs Passing Touchdowns Per Game", subtitle = "    Among LL Passers with 100+ Attempts between 2013-2018") +
#   scale_color_manual("Team", 
#                      values = get_palette(color_count_team))
# 
# LLDataRushCombined <- LLData_clean |>
#   group_by(Player, Team) |>
#   summarize(
#     Rush.Att_total = sum(Rush.Att, na.rm = TRUE),
#     Rush.Yds_total = sum(Rush.Yds),
#     Rush.Yds.Gn_total = sum(Rush.Yds.Gn),
#     ypc = Rush.Yds_total / Rush.Att_total,
#     ygpc = Rush.Yds.Gn_total / Rush.Att_total,
#     Rush.TD_total = sum(Rush.TD, na.rm = TRUE),
#     games_played = sum(GP, na.rm = TRUE),
#     rush_yds_pg = Rush.Yds_total/games_played,
#     rush_tds_pg = Rush.TD_total / games_played,
#     first_season = min(Season, na.rm = TRUE),
#   ) |>
#   filter(is.finite(ypc)) |>
#   ungroup()
# 
# LLDataRushFiltered <- LLDataRushCombined |>
#   filter(Rush.Att_total > 199)
# 
# ggplot(LLDataRushFiltered, aes(x = rush_yds_pg, y = rush_tds_pg, color = Team, label = Player)) +
#   geom_point() +
#   theme_minimal() +
#   geom_text_repel() +
#   geom_vline(xintercept = mean(LLDataRushFiltered$rush_yds_pg), linetype = "dashed", color = "black") +
#   geom_hline(yintercept = mean(LLDataRushFiltered$rush_tds_pg), linetype = "dashed", color = "black") +
#   xlab("Rushing Yards Per Game") +
#   ylab("Rushing Touchdowns Per Game") +
#   ggtitle("Rushing Yards Per Game vs Rushing Touchdowns Per Game", subtitle = "    Among LL Rushers with 200+ Attempts between 2013-2018") +
#   scale_color_manual("Team", 
#                      values = get_palette(color_count_team))
# 
# color_count_team <- length(unique(LLDataRushFiltered$Team))
# get_palette <- colorRampPalette(brewer.pal(9,"Set1"))



