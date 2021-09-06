total_result_S1

total_result_S2

total_result_S1_S2

result_fn <- rbind(total_result_S1, total_result_S2, total_result_S1_S2)
result_fn

# Save dataframe as .CSV
write.csv(result_fn, 
          paste0('/home/diego/GITHUP_REPO/Master_Thesis_EAGLE/Original_data/Est_result.csv'), 
          row.names = F, quote = F)