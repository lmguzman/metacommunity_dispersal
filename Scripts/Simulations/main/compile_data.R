library(dplyr)
library(tidyr)
library(stringr)

file_list <- list.files("scenario_data")

all_res <- matrix(NA, nrow = length(file_list), ncol = 12)

prey_min_mat <- matrix(NA, nrow = length(file_list), ncol = 25)
prey_max_mat <- matrix(NA, nrow = length(file_list), ncol = 25)

pred_min_mat <- matrix(NA, nrow = length(file_list), ncol = 25)
pred_max_mat <- matrix(NA, nrow = length(file_list), ncol = 25)

for(i in 1:length(file_list)){
  
  all <- readRDS(paste0("scenario_data/", file_list[i]))
  
  abundances_prey <- all$prey
  
  abundances_pred <- all$pred
  
  pred_f <- abundances_pred%>% 
    filter(time == max(time)) %>% 
    dplyr::select(starts_with("X"))
  
  pred_min <- abundances_pred%>% 
    filter(time %in% 1500:max(time)) %>% 
    dplyr::select(starts_with("X")) %>% 
    apply(2, min)
  
  pred_min_mat[i,] <- pred_min
  
  pred_max <- abundances_pred%>% 
    filter(time %in% 1500:max(time)) %>% 
    dplyr::select(starts_with("X")) %>% 
    apply(2, max)
  
  pred_max_mat[i,] <- pred_max
  
  prey_f <- abundances_prey%>%
    filter(time == max(time)) %>%
    dplyr::select(starts_with("X"))
  
  prey_min <- abundances_prey%>% 
    filter(time %in% 1500:max(time)) %>% 
    dplyr::select(starts_with("X")) %>% 
    apply(2, min)
  
  prey_min_mat[i,] <- prey_min
  
  prey_max <- abundances_prey%>% 
    filter(time %in% 1500:max(time)) %>% 
    dplyr::select(starts_with("X")) %>% 
    apply(2, max)
  
  prey_max_mat[i,] <- prey_max
  
  prey_t <- abundances_prey %>%
    gather(key = 'patch', value = 'prey', -time)
  
  prey_t_x1 <- prey_t[which(prey_t$patch == 'X1'),]
  
  prey_t_x1$time_chuncks <- c(0, rep(1:20, each = 100))
  
  am_prey <- prey_t_x1 %>% 
    group_by(time_chuncks) %>% 
    summarise(min_prey = min(prey), max_prey = max(prey), amplitude = max_prey - min_prey)
  
  am_prey <- prey_t_x1 %>% 
    group_by(time_chuncks) %>% 
    summarise(min_prey = min(prey), max_prey = max(prey), amplitude = max_prey - min_prey)
  
  prey_tip <- abundances_prey %>% 
    gather(key = 'patch', value = 'tip', -time) 
  
  pred_t <- abundances_pred %>% 
    gather(key = 'patch', value = 'pred', -time) 
  
  final_abundance <-left_join(prey_tip, pred_t) %>% 
    filter(time == 2000) %>% 
    select(tip, pred)
  
  final_abundance[final_abundance>0] <- 1
  
  cs <- colSums(final_abundance)
  
  r1 <- any(pred_f > 0)
  r2 <- rowMeans(pred_f)
  r3 <- r2 < 1000
  r4 <- any(prey_f > 0)
  r5 <- am_prey[am_prey$time_chuncks == 15,4] > am_prey[am_prey$time_chuncks == 5,4]
  
  names <- unlist(str_split(file_list[i], "_"))
  
  k_name <- unlist(str_split(names[10], "\\."))
  
  all_res[i,] <- c(as.numeric(r1), as.numeric(r2), as.numeric(r3), as.numeric(r4), as.numeric(r5), as.numeric(cs[1]), as.numeric(cs[2]), 
    as.numeric(names[2]), as.numeric(names[4]), as.numeric(names[6]), as.numeric(names[8]), as.numeric(k_name[1]))
  
}

all_res_df <- as.data.frame(all_res)

colnames(all_res_df) <- c("pred_g_0", "mean_pred_abun", "pred_l_1000", "prey_g_0", "prey_am_dec", "prey_ocu",  "pred_ocu",
                          "r", "K", "B", "rp", "Kp")

write.csv(all_res_df, "main_res_data.csv", row.names = FALSE)
write.csv(prey_min_mat, "prey_min_mat_data.csv", row.names = FALSE)
write.csv(pred_min_mat, "pred_min_mat_data.csv", row.names = FALSE)
write.csv(prey_max_mat, "prey_max_mat_data.csv", row.names = FALSE)
write.csv(pred_max_mat, "pred_max_mat_data.csv", row.names = FALSE)



