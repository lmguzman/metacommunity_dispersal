library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(purrr)

dirs_all <- list.dirs("output_c_15_mnc_0.1/")

sub_dir <- dirs_all[nchar(dirs_all) >46 & nchar(dirs_all) <60] 

clusts <- list()

for(i in 1:length(sub_dir)){
  
  cv_tbl <- read.table(paste0(sub_dir[i],"/admixture_results/cv_results.txt"), sep = "\t")
  
  cv_res <- cv_tbl %>% 
    mutate(K = 1:5) %>% 
    separate(V1, c('b', 'cv'), ":") %>% 
    mutate(cv = as.numeric(cv)) %>% 
    ggplot(aes(x = K, y = cv)) + geom_point() + geom_line()
  
  ggsave(cv_res, filename = paste0(sub_dir[i],"/admixture_results/cv_plot.jpeg"))
  
  inds <- read.table(paste0(sub_dir[i],"/ids.txt")) %>% 
    rename(ind_id = V1)
  
  ids_dir <- read.table(paste0(sub_dir[i],"/ind_dir.txt")) %>% 
    rename(ind_id = V1, sam_id = V2, pop = V3)
  
  new_pops <- inds %>% 
    left_join(ids_dir) %>% 
    select(sam_id, pop)
  
  write.table(new_pops, paste0(sub_dir[i],"/pops.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  tbl <- read.table(paste0(sub_dir[i],"/admixture_results/admix_3.Q"))
  
  clus <- cbind(tbl,inds) %>% 
    left_join(ids_dir) %>% 
    gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
    arrange(pop) %>% 
    mutate(sam_id = factor(sam_id))
  
  clus_plot <- clus %>% 
    ggplot(aes(x = sam_id, y = Ancestry, fill = cluster)) +geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, size = 7),
          panel.spacing = unit(0, "lines"), 
          strip.background = element_blank(),
          strip.placement = "outside", 
          legend.position = 'none') + xlab("") + 
    facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 

  ggsave(clus_plot, filename = paste0(sub_dir[i],"/admixture_results/clus_plot.jpeg"))
}



pops <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/pops_ordered") %>% 
  rename(id = V1, pop = V2)

cbind(tbl, pops) %>% 
  gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
  arrange(pop) %>% 
  mutate(id = factor(id)) %>% 
  ggplot(aes(x = id, y = Ancestry, fill = cluster)) +geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, size = 7),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = 'none') + xlab("") + 
  facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 






