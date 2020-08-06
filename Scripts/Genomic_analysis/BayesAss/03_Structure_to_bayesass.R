library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

sep_cols_snps <- function(x){
  sel_cols_u <- shuf_snps[,c(paste0("L", x))]
  split_l_df <- str_split(sel_cols_u, pattern = "_") %>% 
    map_df(~data.frame(loc = paste0("L", x), L1 = .x[1], L2 = .x[2]), .id = "ind_count")
 return(split_l_df) 
}


sep_cols_snps_all <- function(x){
  sel_cols_u <- snps[,c(paste0("L", x))]
  split_l_df <- str_split(sel_cols_u, pattern = "_") %>% 
    map_df(~data.frame(loc = paste0("L", x), L1 = .x[1], L2 = .x[2]), .id = "ind_count")
  return(split_l_df) 
}


snps_r <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/structureSNP.txt")

snps <- snps_r

colnames(snps) <- c("ind", "pop", paste0("L", rep(1:(ncol(snps)-2))))

l_sep <- lapply(1:(ncol(snps)-2), sep_cols_snps_all)

united_snps <- bind_rows(l_sep)

united_snps %>% nrow()

bayesass_snps <- data.frame(ind = rep(snps_r$V1, (ncol(snps_r)-2)), pop = rep(snps_r$V2, (ncol(snps_r)-2)), united_snps) %>% 
  dplyr::select(-ind_count)

bayesass_snps$L1 <- as.numeric(bayesass_snps$L1)

bayesass_snps$L2 <- as.numeric(bayesass_snps$L2)

bayesass_snps %>% head()

write.table(bayesass_snps, "output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/all_bayesass.txt", col.names = FALSE, row.names = FALSE, quote = FALSE)






for(i in 1:100){
  
  shuf_cols <- sample(3:ncol(snps), 200, replace = FALSE)
  
  sel_cols <- sort(c(shuf_cols))
  
  shuf_snps <- snps[, c(1, 2, sel_cols)]
  
  colnames(shuf_snps) <- c("ind", "pop", paste0("L", rep(1:200)))
  
  l_sep <- lapply(1:200, sep_cols_snps)
  
  united_snps <- bind_rows(l_sep)
  
  bayesass_snps <- data.frame(ind = rep(snps$V1, 200), pop = rep(snps$V2, 200), united_snps) %>% 
    dplyr::select(-ind_count)
  
  bayesass_snps$L1 <- as.numeric(bayesass_snps$L1)
  
  bayesass_snps$L2 <- as.numeric(bayesass_snps$L2)
  
  name <- paste0("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/bayesass_files/bayesass_snps",i,".txt")
  
  write.table(bayesass_snps, name, col.names = FALSE, row.names = FALSE, quote = FALSE)
  
}




