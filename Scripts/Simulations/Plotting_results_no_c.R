library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)

final_df <- read.csv("model/no_c_outputs/main_res_data.csv")

both_not_persist <- final_df %>%
  filter(pred_g_0 == 0 & prey_g_0 == 0) %>% nrow()/nrow(final_df) 

prey_persists <- final_df %>% 
  filter(prey_g_0 == 1 & pred_g_0 == 0) %>% nrow()/nrow(final_df) 

pred_persists <- final_df %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 0) %>% nrow()/nrow(final_df)

both_persist_same_ocu <- final_df %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 1  & 
           prey_ocu ==  pred_ocu) %>% nrow()/nrow(final_df)

both_persist_diff_ocu <- final_df %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 1  & 
           prey_ocu !=  pred_ocu) %>% nrow()/nrow(final_df)

both_persist_am_increase <- final_df %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 1 & prey_am_dec == 1) %>% nrow()/nrow(final_df)

final_df %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 1 & prey_am_dec == 0 & 
           prey_ocu ==  pred_ocu) %>% 
  filter(mean_pred_abun > 0.5 & mean_pred_abun < 3) %>% 
  arrange(mean_pred_abun) %>% slice(500)

both_not_persist + prey_persists + pred_persists + both_persist_same_ocu + both_persist_diff_ocu 

data_per <- data.frame(percentage = c(both_not_persist, prey_persists, pred_persists, both_persist_same_ocu,
                                     both_persist_diff_ocu),
                       condition = c("No survival", "Only prey \n survives", "Only predator \n survives", "Prey and predator survive \n with same occupancy",
                                     "Prey and predator survive \n with different occupancy"),
                      scenario = 'data')


## reverse 

final_df_rev <- read.csv("model/no_c_outputs/main_res_reverse.csv")

both_not_persist_rev <- final_df_rev %>%
  filter(pred_g_0 == 0 & prey_g_0 == 0) %>% nrow()/nrow(final_df_rev) 

prey_persists_rev <- final_df_rev %>% 
  filter(prey_g_0 == 1 & pred_g_0 == 0) %>% nrow()/nrow(final_df_rev) 

pred_persists_rev <- final_df_rev %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 0) %>% nrow()/nrow(final_df_rev)

both_persist_same_ocu_rev <- final_df_rev %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 1  & 
           prey_ocu ==  pred_ocu) %>% nrow()/nrow(final_df_rev)

both_persist_diff_ocu_rev <- final_df_rev %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 1  & 
           prey_ocu !=  pred_ocu) %>% nrow()/nrow(final_df_rev)

both_persist_am_increase_rev <- final_df_rev %>% 
  filter(pred_g_0 == 1 & prey_g_0 == 1 & prey_am_dec == 1) %>% nrow()/nrow(final_df_rev)

both_not_persist_rev + prey_persists_rev + pred_persists_rev + both_persist_same_ocu_rev + 
  both_persist_diff_ocu_rev + both_persist_am_increase_rev


rev_per <- data.frame(percentage = c(both_not_persist_rev, prey_persists_rev, pred_persists_rev, both_persist_same_ocu_rev, 
    both_persist_diff_ocu_rev),
    condition = c("No survival", "Only prey \n survives", "Only predator \n survives", "Prey and predator survive \n with same occupancy",
                  "Prey and predator survive \n with different occupancy"),
    scenario = 'reverse')

all_scenarios <- rbind(data_per, rev_per) %>% 
  left_join(data.frame(scenario = c("data", "reverse"), scenario_nice = 
                         c("Observed", "Reverse"))) %>% 
  ggplot(aes(x = scenario_nice, y = percentage, fill = condition)) + geom_bar(stat = 'identity') +
  facet_wrap(~condition, scales = "free_x") +
  theme_half_open()+
  theme(legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(margin = margin(0.1,0,0.1,0, "cm"), size = 17),
        axis.text = element_text(size = 17),
        axis.title = element_text(size =20)) +
  xlab("Direction of dispersal difference") + ylab("Proportion of simulation runs")



### abundance data 

coordinates <- read.csv('Other_data/Site_coor.csv', header = TRUE)

coordinates[7,3] <- -22.29499


species <- read.csv("model/raw_data/species.csv")

original_occupancy <- species %>%
  select(Tipulidae, Leptagrion) %>% 
  mutate(Site = rep(1:10, each= 10)) %>% 
  group_by(Site) %>% 
  summarise(Prey = sum(Tipulidae), Predator = sum(Leptagrion)) 

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggsn)
library(scatterpie)

pie_data <- coordinates %>% 
  left_join(original_occupancy) %>% 
  mutate(orig_CentioidLongitude = CentioidLongitude, orig_CentrioidLatitude = CentrioidLatitude) %>% 
  mutate(CentioidLongitude = CentioidLongitude+c(0.3, 0.75, 1, 0.9, 0.45, 0, 0.1, 0,0,0)) %>% 
  mutate(CentrioidLatitude = CentrioidLatitude +c(0.6, 0.6, 0.2, -0.3, -0.3, 0.3, -0.1,0,0,0))

brasil <- map_data('worldHires', "brazil")

ggplot() + geom_polygon(data = brasil, aes(x=long, y = lat, group = group), fill = '#B7C3F3', colour = NA, size = 0.2) + 
  coord_fixed(ratio = 1.1, xlim = c(-46,-40), ylim = c(-25, -21)) + 
  geom_segment(data = pie_data, aes(x = orig_CentioidLongitude, xend = CentioidLongitude, y = orig_CentrioidLatitude, yend = CentrioidLatitude)) +
  geom_scatterpie(data =pie_data, aes(x = CentioidLongitude, y = CentrioidLatitude, r=0.2), cols = c('Prey', "Predator")) +
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = 15)) + scale_fill_discrete(name = "")



######### examples of scenarios ######

#source("model/model_without_C.R")

source("model/model_ricker.R")

## stable coexistence all patches the same  

patch_df <- data.frame(patch = paste0("X", 1:4), nice_p = paste("Patch", 1:4))

rs <- 1.61
Ks <-22
B <-  0.8
rp <-  0.311
kp <-9

df_all <- expand.grid(rs, Ks, B, rp, kp)

m1 <- run_model(1, df_all)

prey_df <- m1[[1]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'prey')

predator_df <- m1[[2]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'pred')

stable_all_same <- rbind(prey_df, predator_df) %>% 
  filter(patch %in% paste0("X", 1:4), time %in% 0:50) %>% 
  left_join(patch_df) %>% 
  ggplot(aes(x = time,y = abundance, colour = sp)) + facet_wrap(~nice_p, nrow = 1) + geom_line() +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none') + ylab("Abundance") + xlab("") +
  scale_x_continuous(breaks = c(0, 25, 50))
  
###### stable coexistence different occupancy ###

rs <- 1.61
Ks <-22
B <-  0.2
rp <-  0.141
kp <-5

df_all <- expand.grid(rs, Ks, B, rp, kp)

m1 <- run_model(1, df_all)

prey_df <- m1[[1]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'prey')

predator_df <- m1[[2]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'pred')

stable_diff_ocu <- rbind(prey_df, predator_df) %>% 
  filter(patch %in% paste0("X", c(5,6,7,11)), time %in% 0:50) %>% 
  #left_join(patch_df) %>% 
  ggplot(aes(x = time,y = abundance, colour = sp)) + 
  #facet_wrap(~nice_p, nrow = 1) + 
  facet_wrap(~patch, nrow = 1) +
  geom_line() +
  theme_cowplot()+
  theme(strip.background = element_blank(),
        legend.position = 'none',
        strip.text.x = element_blank()) + ylab("Abundance") + xlab("")+
  scale_x_continuous(breaks = c(0, 25, 50))

######### biggest oscillation ######

rs <- 2.41
Ks <-22
B <-  0.3
rp <-  0.041
kp <-6

df_all <- expand.grid(rs, Ks, B, rp, kp)

m1 <- run_model(1, df_all)

prey_df <- m1[[1]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'prey')

predator_df <- m1[[2]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'pred')

big_oscillation <- rbind(prey_df, predator_df) %>% 
  filter(patch %in% paste0("X", 1:4), time %in% c(0:50)) %>%
  left_join(patch_df) %>% 
  ggplot(aes(x = time,y = abundance, colour = sp)) + facet_wrap(~nice_p, nrow = 1) + geom_line() +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        strip.text.x = element_blank()) + ylab("Abundance") + xlab("")+
  scale_x_continuous(breaks = c(0, 25, 50))

###### oscilating but stabilizing diff occupancy #######

rs <- 2.01
Ks <-22
B <-  0.1
rp <-  0.091
kp <-9

final_df$B %>% unique()

rs <- 2.01
Ks <-22
B <-  0.1
rp <-  0.091
kp <-5


df_all <- expand.grid(rs, Ks, B, rp, kp)

m1 <- run_model(1, df_all)

prey_df <- m1[[1]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'prey')

predator_df <- m1[[2]] %>% 
  gather(key = patch, value = abundance, -time) %>% 
  mutate(sp = 'pred')


ocsi_diff <- rbind(prey_df, predator_df) %>% 
  filter(patch %in% paste0("X", c(1, 10, 5, 7)), time %in% 1:50) %>% 
  ggplot(aes(x = time,y = abundance, colour = sp)) + facet_wrap(~patch, nrow = 1) + geom_line() +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        strip.text.x = element_blank()) + ylab("Abundance") + xlab("Time steps") +
  scale_x_continuous(breaks = c(0, 25, 50))



all_together <- plot_grid(stable_all_same, stable_diff_ocu, big_oscillation, ocsi_diff, ncol = 1, 
                          labels = c("a.", "b.", "c.", "d."))

ggsave(all_together, filename = "Figures/example_dynamics.jpeg", height = 10, width = 8)




###### grid ####

distance_all <- as.matrix(read.csv("Other_data/distance_all.csv"))

distance_from_each_patch <- as.data.frame(distance_all) %>% 
  mutate(patch_from= 1:25) %>% 
  gather(key = patch, value = distance, -patch_from) %>% 
  mutate(patch = as.numeric(str_remove(patch, "X"))) %>% 
  ggplot(aes(x = patch, y = patch_from, fill = distance)) + geom_tile() + ylab("Patch number") + xlab("Patch number") + scale_fill_continuous(name = 'Distance') +
  theme_cowplot() + scale_x_continuous(breaks = 1:25) + scale_y_continuous(breaks = 1:25)


grid <- data.frame(expand.grid(1:5, 1:5), lab = 1:25) %>% 
  mutate(col = case_when(lab %in% c(1,21,5,25) ~ "a",
                         lab %in% c(22, 23, 24, 2, 3, 4) ~ "b",
                         lab %in% c(6, 11, 16, 10, 15, 20) ~ "c", 
                         lab %in% c(17, 18, 19, 12, 13, 14, 7, 8, 9) ~ "d")) %>% 
  ggplot(aes(x = Var1, y = Var2, colour = col)) + geom_text(aes(label = lab)) +
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = 'none') + xlab("x") + ylab("y") + 
  scale_colour_manual(values = c("#F2542D", "#717EC3", '#5FAD41', "#050609"))



######### bifurcation plots ########

prey_min <- read.csv("model/no_c_outputs/pred_min_mat_data.csv")
prey_max <- read.csv("model/no_c_outputs/pred_max_mat_data.csv")


per_min_max <- cbind(final_df[,8:12], min_p1 = prey_min[, 1], max_p1 = prey_max[, 1])

## stable coexistence
scaleFUN <- function(x) sprintf("%.2f", x)

rs <- 1.61
Ks <-22
B2 <-  0.8
rp2 <-  0.311
kp2 <-9

sc_kp <- per_min_max %>%
  filter(B == B2, rp == rp2, r == rs, K == Ks) %>% 
  ggplot() + geom_point(aes(x = Kp, y = min_p1)) +
  geom_point(aes(x = Kp, y = max_p1)) + ylab("") + xlab(expression(K[p]))

sc_b <- per_min_max %>%
  filter(rp == rp2, r == rs, K == Ks, Kp == kp2) %>% 
  ggplot() + geom_point(aes(x = B, y = min_p1)) +
  geom_point(aes(x = B, y = max_p1)) + ylab("") + xlab("B")

sc_rp <- per_min_max %>%
  filter(r == rs, K == Ks, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = rp, y = min_p1)) +
  geom_point(aes(x = rp, y = max_p1)) + ylab("") + xlab(expression(r[p]))+ 
  scale_y_continuous(labels=scaleFUN)

sc_r <- per_min_max %>%
  filter(rp == rp2, K == Ks, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = r, y = min_p1)) +
  geom_point(aes(x = r, y = max_p1))+ ylab("") + xlab("r")

sc_k <- per_min_max %>%
  filter(rp == rp2, r == rs, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = K, y = min_p1)) +
  geom_point(aes(x = K, y = max_p1)) + ylab("") + xlab("K")


### big oscillations

rs <- 2.41
Ks <-22
B2 <-  0.3
rp2 <-0.041
kp2 <-7

bo_kp <- per_min_max %>%
  filter(B == B2, rp == rp2, r == rs, K == Ks) %>% 
  ggplot() + geom_point(aes(x = Kp, y = min_p1)) +
  geom_point(aes(x = Kp, y = max_p1)) + ylab("") + xlab(expression(K[p]))

bo_b <- per_min_max %>%
  filter(rp == rp2, r == rs, K == Ks, Kp == kp2) %>% 
  ggplot() + geom_point(aes(x = B, y = min_p1)) +
  geom_point(aes(x = B, y = max_p1)) + ylab("") + xlab("B")

bo_rp <- per_min_max %>%
  filter(r == rs, K == Ks, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = rp, y = min_p1)) +
  geom_point(aes(x = rp, y = max_p1)) + ylab("") + xlab(expression(r[p]))+ 
  scale_y_continuous(labels=scaleFUN)

bo_r <- per_min_max %>%
  filter(rp == rp2, K == Ks, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = r, y = min_p1)) +
  geom_point(aes(x = r, y = max_p1))+ ylab("") + xlab("r")

bo_k <- per_min_max %>%
  filter(rp == rp2, r == rs, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = K, y = min_p1)) +
  geom_point(aes(x = K, y = max_p1)) + ylab("") + xlab("K")



### small oscillations

rs <- 2.01
Ks <-22
B2 <-  0.1
rp2 <-  0.091
kp2 <-9

so_kp <- per_min_max %>%
  filter(B == B2, rp == rp2, r == rs, K == Ks) %>% 
  ggplot() + geom_point(aes(x = Kp, y = min_p1)) +
  geom_point(aes(x = Kp, y = max_p1)) + ylab("") + xlab(expression(K[p]))

so_b <- per_min_max %>%
  filter(rp == rp2, r == rs, K == Ks, Kp == kp2) %>% 
  ggplot() + geom_point(aes(x = B, y = min_p1)) +
  geom_point(aes(x = B, y = max_p1)) + ylab("") + xlab("B")


so_rp <- per_min_max %>%
  filter(r == rs, K == Ks, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = rp, y = min_p1)) +
  geom_point(aes(x = rp, y = max_p1)) + ylab("") + xlab(expression(r[p])) + 
  scale_y_continuous(labels=scaleFUN)

so_r <- per_min_max %>%
  filter(rp == rp2, K == Ks, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = r, y = min_p1)) +
  geom_point(aes(x = r, y = max_p1))+ ylab("") + xlab("r")

so_k <- per_min_max %>%
  filter(rp == rp2, r == rs, Kp == kp2, B == B2) %>% 
  ggplot() + geom_point(aes(x = K, y = min_p1)) +
  geom_point(aes(x = K, y = max_p1)) + ylab("") + xlab("K")


bifurcation <- plot_grid(sc_kp, bo_kp, so_kp, sc_rp, bo_rp, so_rp,
           sc_b, bo_b, so_b, sc_k, bo_k, so_k, sc_r, bo_r, so_r, ncol = 3)

bifurcation_lab <- bifurcation + draw_label("Min/Max predator abundance", x = 0.015, y = 0.55, angle = 90)

ggsave(bifurcation_lab, filename = "Figures/bifurcation.jpeg", height = 10, width = 7)


