LLData_final_fixed <- LLData_final[-(4344:4358), ]
LLData_final_fixed <- LLData_final_fixed %>% 
  slice(-(4766:4794))
LLData_final_fixed <- LLData_final_fixed[-c(4751,4752,4753,4754,4755,4756,4757,4763,4764,4765), ]
LLData_final_fixed <- LLData_final_fixed[-c(3919,3969,4029,4094,5702), ]

library(dplyr)
LLData_final_fixed_clean <- LLData_final_fixed |>
  distinct(Player,Season, .keep_all = TRUE)
LLDataCombined <- LLData_final_fixed_clean |>
  group_by(Player, Team) |>
  summarize(
    games_played = sum(GP, na.rm = TRUE),
    first_season = min(Season, na.rm = TRUE),
  ) |>
  ungroup()

write.csv(LLData_final_fixed_clean, "LLData_final.csv", row.names = FALSE)
