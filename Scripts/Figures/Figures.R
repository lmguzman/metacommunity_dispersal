library(dplyr)
library(tidyr)
### Figure 1 the map and sampling regime

coordinates <- read.csv('Other_data/Site_coor.csv', header = TRUE)

coordinates[7,3] <- -22.29499

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggsn)


brasil <- map_data('worldHires', "brazil")

brasil$subregion %>% unique()

b1 <- ggplot() + geom_polygon(data = brasil, aes(x=long, y = lat, group = group), fill = '#5D576B', colour = NA, size = 0.1) + 
  coord_fixed(1.2) + theme_nothing() + theme(panel.background = element_rect(fill = "#E6EBE0")) +
  geom_point(data = coordinates, aes(x = CentioidLongitude, y = CentrioidLatitude), colour = '#ED6A5A', size = 3.5)

l2 <- data.frame(site = c(rep(" ",4), "1-5", '6', '7', '8', '9', '10'), lon = coordinates$CentioidLongitude+ c(rep(0, 4), 0.15, 0.09,0, -0.05, 0, 0.23),
                 lat = coordinates$CentrioidLatitude + c(rep(-0.20, 5), rep(0.2, 4), 0))

b2 <- ggplot() + geom_polygon(data = brasil, aes(x=long, y = lat, group = group), fill = '#5D576B', colour = NA, size = 0.2) + 
  coord_fixed(ratio = 1.1, xlim = c(-46,-40), ylim = c(-25, -21)) + theme_nothing() + theme(panel.background = element_rect(fill = "#E6EBE0")) +
  geom_point(data = coordinates, aes(x = CentioidLongitude, y = CentrioidLatitude), colour = '#ED6A5A', size = 3) +
  geom_text(data = l2, aes(x = lon, y = lat, label = site),
            size = 10) +
  scalebar(brasil, dist = 100, dd2km = FALSE, model = 'WGS84', anchor = c(x= -42, y = -23.4), height = 0.001, st.size = 8, st.dist = 0.005)  


l3 <- data.frame(site = c(rep(" ",3), "1-4",'5', '6', '7', '8', '9', '10'), lon = coordinates$CentioidLongitude, lat = coordinates$CentrioidLatitude + 0.02)

b3 <- ggplot() + geom_polygon(data = brasil, aes(x=long, y = lat, group = group), fill = '#5D576B', colour = NA, size = 0.2) + 
  coord_fixed(ratio = 1.1, xlim=c(-41.71,-41.45), ylim = c(-22.35, -22.15)) + theme_nothing()  + theme(panel.background = element_rect(fill = "#E6EBE0")) +
  geom_point(data = coordinates, aes(x = CentioidLongitude, y = CentrioidLatitude), colour = '#ED6A5A', size = 3) +
  geom_text(data = l3, aes(x = lon, y = lat, label = site), size = 10) + 
  scalebar(brasil, dist = 5, dd2km = FALSE, model = 'WGS84', anchor = c(x= -41.51, y = -22.26), height = 0.00005, st.size = 8, st.dist = 0.0003) 


b4 <- ggplot() + geom_polygon(data = brasil, aes(x=long, y = lat, group = group), fill = '#5D576B', colour = NA,size = 0.2) + 
  coord_fixed(ratio = 1.1, xlim=c(-41.53,-41.48), ylim = c(-22.24, -22.19)) + theme_nothing()  + theme(panel.background = element_rect(fill = "#E6EBE0"))+
  geom_point(data = coordinates, aes(x = CentioidLongitude, y = CentrioidLatitude), colour = '#ED6A5A', size = 3) +
  geom_text(data = coordinates, aes(x = CentioidLongitude, y = CentrioidLatitude + 0.003, label = Site), size = 10) + 
  scalebar(brasil, dist = 1, dd2km = FALSE, model = 'WGS84', anchor = c(x= -41.49, y = -22.229), height = 0.000015, st.size = 8, st.dist = 0.00004) 

ggsave(b1, filename = 'Figures/MAP/b1.jpeg')
ggsave(b2, filename = 'Figures/MAP/b2.jpeg')
ggsave(b3, filename = 'Figures/MAP/b3.jpeg')
ggsave(b4, filename = 'Figures/MAP/b4.jpeg')


###### Appendix figure cv and loglikelihood for big clusters
library(dplyr)

all_cv <- data.frame()

for(i in 1:5){
  
  t1 <- read.table(paste0("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/admixture_results/likelihood_results_", i, ".txt"), header = FALSE, fill = TRUE)
  
  df_t <- data.frame(ll = as.numeric(as.character(t1$V2[seq(1,9, 2)])), cv = t1$V4[seq(2,10,2)], r =i, K = 1:5)
  
  all_cv <- bind_rows(all_cv, df_t)
}

cv_d <- all_cv %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = cv)) +
  ylab('Cross validation error') +
  theme(legend.position = 'none')

ll_d <- all_cv %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = ll))+
  ylab('Loglikelihood') +
  theme(legend.position = 'none')


all_cv_t <- data.frame()

for(i in 1:5){
  
  t1 <- read.table(paste0("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/admixture_results/likelihood_results_", i, ".txt"), header = FALSE, fill = TRUE)
  
  df_t <- data.frame(ll = as.numeric(as.character(t1$V2[seq(1,9, 2)])), cv = t1$V4[seq(2,10,2)], r =i, K = 1:5)
  
  all_cv_t <- bind_rows(all_cv_t, df_t)
}

cv_t <- all_cv_t %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = cv)) +
  ylab('Cross validation error') +
  theme(legend.position = 'none')

ll_t <- all_cv_t %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = ll))+
  ylab('Loglikelihood') +
  theme(legend.position = 'none')

cv_all <- plot_grid(cv_d, cv_t, ll_d, ll_t)

ggsave(cv_all, filename = "Figures/cv_gen.jpeg")


##### Figure 2 admixture results for the tipulid and the damselfly for the general cluster

### damselfly

t1 <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/admixture_results/run_1/admix_2.Q", header = FALSE, fill = TRUE)

pops <- pops <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/pops_ordered") %>% 
  rename(id = V1, pop = V2)

dam <- cbind(t1, pops) %>% 
  arrange(pop) %>%
  mutate(id2 = factor(1:95)) %>% 
  gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
  mutate(id = factor(id)) %>% 
  ggplot(aes(x = id2, y = Ancestry, fill = cluster)) +geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, size = 7),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = 'none',
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25)) + xlab("Population") + 
  facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 

ggsave(dam, filename = "Figures/dam_gen_clus.jpeg")




### Tipulid

t1 <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/admixture_results/run_1/admix_2.Q", header = FALSE, fill = TRUE)

pops <- pops <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/pops_ordered") %>% 
  rename(id = V1, pop = V2)

tip <- cbind(t1, pops) %>% 
  arrange(pop) %>%
  mutate(id2 = factor(1:95)) %>% 
  gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
  mutate(id = factor(id)) %>% 
  ggplot(aes(x = id2, y = Ancestry, fill = cluster)) +geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, size = 7),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = 'none',
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25)) + xlab("Population") + 
  facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 

ggsave(tip, filename = "Figures/tip_gen_clus.jpeg")



##### Figure S3 cross validation error of the c1 and c2 for the damselfly and the tipulid #######

####### damselfly

all_cv_1 <- data.frame()

for(i in 1:5){
  
  t1 <- read.table(paste0("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/admixture_results/likelihood_results_", i, ".txt"), header = FALSE, fill = TRUE)
  
  df_t <- data.frame(ll = as.numeric(as.character(t1$V2[seq(1,9, 2)])), cv = t1$V4[seq(2,10,2)], r =i, K = 1:5)
  
  all_cv_1 <- bind_rows(all_cv_1, df_t)
}

cv_d_1 <- all_cv_1 %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = cv)) +
  ylab('Cross validation error') +
  theme(legend.position = 'none')


all_cv_2 <- data.frame()

for(i in 1:5){
  
  t1 <- read.table(paste0("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c2/admixture_results/likelihood_results_", i, ".txt"), header = FALSE, fill = TRUE)
  
  df_t <- data.frame(ll = as.numeric(as.character(t1$V2[seq(1,9, 2)])), cv = t1$V4[seq(2,10,2)], r =i, K = 1:5)
  
  all_cv_2 <- bind_rows(all_cv_2, df_t)
}

cv_d_2 <- all_cv_2 %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = cv)) +
  ylab('Cross validation error') +
  theme(legend.position = 'none')




####### Tipulid

all_ct_1 <- data.frame()

for(i in 1:5){
  
  t1 <- read.table(paste0("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/admixture_results/likelihood_results_", i, ".txt"), header = FALSE, fill = TRUE)
  
  df_t <- data.frame(ll = as.numeric(as.character(t1$V2[seq(1,9, 2)])), cv = t1$V4[seq(2,10,2)], r =i, K = 1:5)
  
  all_ct_1 <- bind_rows(all_ct_1, df_t)
}

cv_t_1 <- all_ct_1 %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = cv)) +
  ylab('Cross validation error') +
  theme(legend.position = 'none')


all_ct_2 <- data.frame()

for(i in 1:5){
  
  t1 <- read.table(paste0("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c2/admixture_results/likelihood_results_", i, ".txt"), header = FALSE, fill = TRUE)
  
  df_t <- data.frame(ll = as.numeric(as.character(t1$V2[seq(1,9, 2)])), cv = t1$V4[seq(2,10,2)], r =i, K = 1:5)
  
  all_ct_2 <- bind_rows(all_ct_2, df_t)
}

cv_t_2 <- all_ct_2 %>% 
  ggplot(aes(x = K, group = r, colour = factor(r))) + geom_line(aes(y = cv)) +
  ylab('Cross validation error') +
  theme(legend.position = 'none')

cv_all_s <- plot_grid(cv_d_1, cv_d_2, cv_t_1, cv_t_2)


ggsave(cv_all_s, filename = 'Figures/cv_all_c1_c2.jpeg')


####### Figure S1 histograms of IBS #####

dam_tb <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/IBS_distance.txt",skip = 5, sep = "\t") %>% 
  dplyr::select(-V1)

hist_dam <- data.frame(ibs = dam_tb[upper.tri(dam_tb)]) %>% 
  ggplot(aes(x = ibs)) + geom_histogram() +  ylab("Frequency") + xlab("1 - IBS")


tip_tb <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/IBS_distance.txt",skip = 5, sep = "\t") %>% 
  dplyr::select(-V1)

hist_tip <- data.frame(ibs = tip_tb[upper.tri(tip_tb)]) %>% 
  ggplot(aes(x = ibs)) + geom_histogram() + ylab("Frequency") + xlab("1 - IBS")


hists <- plot_grid(hist_dam, hist_tip)

ggsave(hists, filename = "Figures/ibs_hist.jpeg")

######## Figure 3 ############

## dam c1 

d1 <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/admixture_results/run_1/admix_2.Q", header = FALSE, fill = TRUE)

pops_d1 <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/pops.txt") %>% 
  rename(id = V1, pop = V2)

d1_plot <- cbind(d1, pops_d1) %>% 
  arrange(pop) %>%
  mutate(id2 = factor(1:nrow(pops_d1))) %>% 
  gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
  mutate(id = factor(id)) %>% 
  ggplot(aes(x = id2, y = Ancestry, fill = cluster)) +geom_bar(stat="identity")  +
  theme(axis.text.x = element_text(angle = 90, size = 10),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = 'none',
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25)) + xlab("") + 
  facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 

ggsave(d1_plot, filename = "Figures/d1_plot_clus.jpeg")




## dam c1 

d2 <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c2/admixture_results/run_1/admix_3.Q", header = FALSE, fill = TRUE)

id <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c2/ids.txt") 

inds <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c2/ind_dir.txt") 

pops_d2 <- inds %>% 
  filter(V1 %in% id$V1) %>% 
  dplyr::select(V2, V3) %>%
  rename(id = V2, pop = V3)
  
d2_plot <- cbind(d2, pops_d2) %>% 
  arrange(pop) %>%
  mutate(id2 = factor(1:nrow(pops_d2))) %>% 
  gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
  mutate(id = factor(id)) %>% 
  ggplot(aes(x = id2, y = Ancestry, fill = cluster)) +geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, size = 10),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = 'none',
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25)) + xlab("Population") + 
  facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 

ggsave(d2_plot, filename = "Figures/d2_plot_clus.jpeg")




## tip c1 

t1 <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/admixture_results/run_1/admix_1.Q", header = FALSE, fill = TRUE)

pops_t1 <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/pops.txt") %>% 
  rename(id = V1, pop = V2)

t1_plot <- cbind(t1, pops_t1) %>% 
  arrange(pop) %>%
  mutate(id2 = factor(1:nrow(pops_t1))) %>% 
  gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
  mutate(id = factor(id)) %>% 
  ggplot(aes(x = id2, y = Ancestry, fill = cluster)) +geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, size = 10),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = 'none',
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 25)) + xlab("") + ylab("")+
  facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 

ggsave(t1_plot, filename = "Figures/t1_plot_clus.jpeg")




## dam c1 

t2 <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c2/admixture_results/run_1/admix_3.Q", header = FALSE, fill = TRUE)

id <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c2/ids.txt") 

inds <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c2/ind_dir.txt") 

pops_t2 <- inds %>% 
  filter(V1 %in% id$V1) %>% 
  dplyr::select(V2, V3) %>%
  rename(id = V2, pop = V3)

t2_plot <- cbind(t2, pops_t2) %>% 
  arrange(pop) %>%
  mutate(id2 = factor(1:nrow(pops_t2))) %>% 
  gather(key = 'cluster', 'Ancestry', starts_with("V")) %>% 
  mutate(id = factor(id)) %>% 
  ggplot(aes(x = id2, y = Ancestry, fill = cluster)) +geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, size = 10),
        panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = 'none',
        strip.text = element_text(size = 20),
        axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 25)) + xlab("Population") + ylab("") +
  facet_grid(~pop, switch = "x", scales = "free_x", space = "free_x") 

ggsave(t2_plot, filename = "Figures/t2_plot_clus.jpeg")




