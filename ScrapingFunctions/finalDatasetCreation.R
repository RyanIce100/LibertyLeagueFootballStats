hil_stats$Season <- as.character(hil_stats$Season)
LLData_new <- bind_rows(rpi_combined,uni_combined,ith_combined,hob_combined,roc_combined,bst_combined,slu_combined,hil_stats)
LLData_new$Season <- as.integer(LLData_new$Season)

LLData_final <- bind_rows(LLData_clean_mergable, LLData_new)
write.csv(LLData_final, "LLData_Final.csv", row.names = FALSE)
write.csv(LLData_clean_mergable, "LLData_Old.csv", row.names = FALSE)
write.csv(LLData_new, "LLData_New.csv", row.names = FALSE)
