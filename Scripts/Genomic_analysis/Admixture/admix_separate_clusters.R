library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(purrr)

dirs_all <- list.dirs("output_c_15_mnc_0.1/")

sub_dir <- dirs_all[nchar(dirs_all) >40 & nchar(dirs_all) < 47] 

clusts <- list()

for(i in 1:length(sub_dir)){
  
  cv_tbl <- read.table(paste0(sub_dir[i],"/admixture_results/cv_results.txt"), sep = "\t")
  
  cv_res <- cv_tbl %>% 
    mutate(K = 1:5) %>% 
    separate(V1, c('b', 'cv'), ":") %>% 
    mutate(cv = as.numeric(cv)) %>% 
    ggplot(aes(x = K, y = cv)) + geom_point() + geom_line()
  
  ggsave(cv_res, filename = paste0(sub_dir[i],"/admixture_results/cv_plot.jpeg"))
 
  tbl <- read.table(paste0(sub_dir[i],"/admixture_results/admix_2.Q"))
  
  jpeg(paste0(sub_dir[i],"/admixture_results/clusters.jpeg"))
  barplot(t(as.matrix(tbl)), col=rainbow(2),
          xlab="Individual #", ylab="Ancestry", border=NA)
  dev.off()
  
  
  inds <- read.table(paste0(sub_dir[i],"/ind_id.txt")) %>% 
    rename(ind = V1)
  
  clus <- tbl %>% 
    mutate(c = ifelse(V1 < 0.5, 2, 1)) %>% 
    cbind(inds) 
  
  ids_c <- split(clus, clus$c) %>% 
    map(~select(.x, ind))
  
  c2_ids <- ids_c[[1]]
  c1_ids <- ids_c[[2]]

  write.table(c2_ids, paste0(sub_dir[i],"/c2/ids.txt"), sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE) 
  write.table(c1_ids, paste0(sub_dir[i],"/c1/ids.txt"), sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE) 
  
}

ids_c1 <- unlist(ids_c[[1]])
names(ids_c1) <- NULL
paste(ids_c1, collapse = ",")
