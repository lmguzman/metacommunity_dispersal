library(dplyr)
library(cowplot)
library(ggplot2)
library(tidyr)
#library(MASS)
library(nlstools)

###### Damselfly #####

migration_rates <- data.frame()

for(i in 1:4){
  
  mig <- read.table(paste0("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/bayesass_outputs/run_",i,"/migration_rates.txt"))
  
  colnames(mig) <- paste0(rep(paste0("to_", 1:8), each = 2), rep(c("_site", "_rate"), 8))
  
  mig_table <- mig %>% 
    mutate(from_site = 1:nrow(mig)) %>% 
    dplyr::select(from_site, contains("rate")) %>% 
    gather(key = "to", value = "value", -from_site) %>% 
    separate(to, c("to", "to_site", "r"),"_") %>% 
    dplyr::select(from_site, to_site, value) %>% 
    separate(value, c("migration_rate", "sd"), sep = "\\(") %>% 
    separate(sd, into = c('sd', 'ext'), sep = '\\)') %>% 
    dplyr::select(-ext) %>% 
    mutate(rep = i)
  
    migration_rates <- bind_rows(migration_rates, mig_table)
}


mean_migration_rates <- migration_rates %>% 
  mutate(to_site = as.numeric(to_site), 
         migration_rate = as.numeric(migration_rate), 
         sd = as.numeric(sd)) %>% 
  group_by(from_site, to_site) %>% 
  summarise(mean_mig = mean(migration_rate), sd = sd(migration_rate)) %>% 
  ungroup() %>% 
  rename(from = from_site, to = to_site) %>% 
  mutate(from_dist = case_when(.$from == 10 ~ 425.666,
                               .$from == 2 ~ 0.2357,
                               .$from == 3 ~ 0.5754,
                               .$from == 4 ~ 1.0294,
                               .$from == 5 ~ 4.8793,
                               .$from == 6 ~ 10.5475,
                               .$from == 7 ~ 23.8796,
                               .$from == 8 ~ 100.7474,
                               .$from == 9 ~ 166.0514,
                               .$from == 1 ~ 0)) %>%
  mutate(to_dist = case_when(.$to == 10 ~ 425.666,
                             .$to == 2 ~ 0.2357,
                             .$to == 3 ~ 0.5754,
                             .$to == 4 ~ 1.0294,
                             .$to == 5 ~ 4.8793,
                             .$to == 6 ~ 10.5475,
                             .$to == 7 ~ 23.8796,
                             .$to == 8 ~ 100.7474,
                             .$to == 9 ~ 166.0514,
                             .$to == 1 ~ 0)) %>% 
  mutate(Diff = to_dist - from_dist) %>% 
  mutate(abs_diff = abs(Diff)) %>% 
  dplyr::select(abs_diff, mean_mig) %>% 
  as.data.frame() %>% 
  filter(abs_diff < 50)


mean_migration_rates %>%
  ggplot(aes(x = abs_diff, y = mean_mig)) + geom_point()  


mean_migration_rates

a_start<-8 #param a is the y value when x=0
b_start<-2*log(2)/a_start

m0<-nls(mean_mig~a*exp(-b*abs_diff),start=list(a=a_start,b=b_start), data = mean_migration_rates)
m1 <- glm(mean_mig~abs_diff, data = mean_migration_rates, family = Gamma(link = "inverse"))
d1 <- m1$deviance/m1$df.residual 
m2 <- glm(mean_mig~abs_diff, data = mean_migration_rates, family = Gamma(link = "log"))
d2 <- m2$deviance/m2$df.residual 

m2 <- glm(mean_mig/d2~abs_diff, data = mean_migration_rates, family = Gamma(link = "log"))
d2 <- m2$deviance/m2$df.residual 

summary(m2)
#### simplest case

formulaExp_0 <- as.formula(mean_mig ~  a*(exp(-b*abs_diff)))

preview(formulaExp_0, data = mean_migration_rates, start = list(b = 0.8, a = 0.8))

tip.nls0 <- nls(formulaExp_0, start = list(b = 0.8, a = 0.8), data = mean_migration_rates)

m0.res1 <- nlsResiduals(tip.nls0)
plot(m0.res1, which = 0)

plotfit(tip.nls0, smooth = TRUE)


### better step case

formulaExp_0.1 <- as.formula(mean_mig ~ (abs_diff == 0) * mig_max + (abs_diff > 0)*a *(exp(-b*abs_diff)))

dam_exp_hur <- nls(formulaExp_0.1, start = list(b = 0.8, mig_max =0.8, a = 0.8), data = mean_migration_rates)

formulaExp_0.1 <- as.formula(mean_mig ~ (abs_diff == 0) * mig_max + (abs_diff > 0)*((1-mig_max)*b) *(exp(-b*abs_diff)))

preview(formulaExp_0.1, data = mean_migration_rates, start = list(b = 0.8, mig_max =0.8))

dam_exp_hur <- nls(formulaExp_0.1, start = list(b = 0.8, mig_max =0.8), data = mean_migration_rates)

dam.res1 <- nlsResiduals(dam_exp_hur)
plot(dam.res1, which = 0)

summary(dam_exp_hur)

overview(dam_exp_hur)

plotfit(dam_exp_hur, smooth = TRUE)

dam.cont1 <- nlsContourRSS(dam_exp_hur)
plot(dam.cont1, col = FALSE, nlev = 5)


AIC(m0,  m2, tip.nls0,dam_exp_hur)


dam_dis_ker <- mean_migration_rates %>% 
  ggplot(aes(x = abs_diff, y = mean_mig)) + geom_point() + 
  geom_line(data = data.frame(abs_diff = mean_migration_rates$abs_diff, predi = predict(dam_exp_hur, type= 'response')),
            aes(x = abs_diff, y = predi)) +
  ylab("Dispersal rate") + xlab("Distance (Km)")

dam_dis_ker_inset <- mean_migration_rates %>% 
  filter(mean_mig < 0.23) %>% 
  ggplot(aes(x = abs_diff, y = mean_mig)) + geom_point() + 
  geom_line(data = filter(data.frame(abs_diff = mean_migration_rates$abs_diff, predi = predict(dam_exp_hur, type= 'response')), abs_diff > 0),
            aes(x = abs_diff, y = predi)) + 
  ylab("") + xlab("") + scale_y_continuous(limits = c(0, 0.06), breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06)) +
  scale_x_continuous(limits = c(0,26), breaks = seq(0,25,5)) +
  theme(axis.text = element_text(size = 30))

ggsave(dam_dis_ker, filename = "Figures/dam_dis_ker.jpeg")
ggsave(dam_dis_ker_inset, filename = "Figures/dam_dis_ker_inset.jpeg", width = 9.2, height = 7.6)



###### Tipulid #####


migration_rates <- data.frame()

for(i in 1:4){
  
  mig <- read.table(paste0("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/bayesass_outputs/run_",i,"/migration_rates.txt"))
  
  colnames(mig) <- paste0(rep(paste0("to_", 1:8), each = 2), rep(c("_site", "_rate"), 8))
  
  mig_table <- mig %>% 
    mutate(from_site = 1:nrow(mig)) %>% 
    dplyr::select(from_site, contains("rate")) %>% 
    gather(key = "to", value = "value", -from_site) %>% 
    separate(to, c("to", "to_site", "r"),"_") %>% 
    dplyr::select(from_site, to_site, value) %>% 
    separate(value, c("migration_rate", "sd"), sep = "\\(") %>% 
    separate(sd, into = c('sd', 'ext'), sep = '\\)') %>% 
    dplyr::select(-ext) %>% 
    mutate(rep = i)
  
  migration_rates <- bind_rows(migration_rates, mig_table)
}


mean_migration_rates <- migration_rates %>% 
  mutate(to_site = as.numeric(to_site), 
         migration_rate = as.numeric(migration_rate), 
         sd = as.numeric(sd)) %>% 
  group_by(from_site, to_site) %>% 
  summarise(migration_rate = mean(migration_rate), sd = sd(migration_rate)) %>% 
  ungroup() %>% 
  rename(from = from_site, to = to_site) %>% 
  mutate(from_dist = case_when(.$from == 10 ~ 425.666,
                               .$from == 2 ~ 0.2357,
                               .$from == 3 ~ 0.5754,
                               .$from == 4 ~ 1.0294,
                               .$from == 5 ~ 4.8793,
                               .$from == 6 ~ 10.5475,
                               .$from == 7 ~ 23.8796,
                               .$from == 8 ~ 100.7474,
                               .$from == 9 ~ 166.0514,
                               .$from == 1 ~ 0)) %>%
  mutate(to_dist = case_when(.$to == 10 ~ 425.666,
                             .$to == 2 ~ 0.2357,
                             .$to == 3 ~ 0.5754,
                             .$to == 4 ~ 1.0294,
                             .$to == 5 ~ 4.8793,
                             .$to == 6 ~ 10.5475,
                             .$to == 7 ~ 23.8796,
                             .$to == 8 ~ 100.7474,
                             .$to == 9 ~ 166.0514,
                             .$to == 1 ~ 0)) %>% 
  mutate(Diff = to_dist - from_dist) %>% 
  mutate(abs_diff = abs(Diff))  %>% 
  dplyr::select(abs_diff,migration_rate) %>% 
  as.data.frame() %>% 
  filter(abs_diff < 50) 


mean_migration_rates %>%
  ggplot(aes(x = abs_diff, y = migration_rate)) + geom_point() 

a_start<-8 #param a is the y value when x=0
b_start<-2*log(2)/a_start

m0.1<-nls(migration_rate~a*exp(-b*abs_diff),start=list(a=a_start,b=b_start), data = fin_mig)
m0<-nls(migration_rate~a*exp(-b*abs_diff),start=list(a=a_start,b=b_start), data = mean_migration_rates)
m1 <- glm(migration_rate~abs_diff, data = mean_migration_rates, family = Gamma(link = "inverse"))
d1 <- m1$deviance/m1$df.residual 
m2 <- glm(migration_rate~abs_diff, data = mean_migration_rates, family = Gamma(link = "log"))
d2 <- m2$deviance/m2$df.residual 
m3 <- glm(migration_rate~abs_diff, data = mean_migration_rates, family = Gamma(link = "identity"))
d3 <- m3$deviance/m3$df.residual 

AIC(m0, m1, m2, m3)
m2 <- glm(migration_rate/d2~abs_diff, data = mean_migration_rates, family = Gamma(link = "log"))
d2 <- m2$deviance/m2$df.residual 

AIC(m2)
library(nlstools)


#### simplest case

formulaExp_0 <- as.formula(migration_rate ~  a*(exp(-b*abs_diff)))

preview(formulaExp_0, data = mean_migration_rates, start = list(b = 0.8, a = 0.8))

tip.nls0 <- nls(formulaExp_0, start = list(b = 0.8, a = 0.8), data = mean_migration_rates)

m0.res1 <- nlsResiduals(tip.nls0)
plot(m0.res1, which = 0)

plotfit(tip.nls0, smooth = TRUE)


### better step case

formulaExp_0.1 <- as.formula(migration_rate ~ (abs_diff == 0) * mig_max + (abs_diff > 0)*a *(exp(-b*abs_diff)))

preview(formulaExp_0.1, data = mean_migration_rates, start = list(b = 0.8, mig_max =0.8, a = 0.8))

tip.nls0.1 <- nls(formulaExp_0.1, start = list(b = 0.8, mig_max =0.8, a = 0.8), data = mean_migration_rates)

formulaExp_0.1 <- as.formula(migration_rate ~ (abs_diff == 0) * mig_max + (abs_diff > 0)*((1-mig_max)*b) *(exp(-b*abs_diff)))

preview(formulaExp_0.1, data = mean_migration_rates, start = list(b = 0.8, mig_max =0.8))

tip.nls0.1 <- nls(formulaExp_0.1, start = list(b = 0.8, mig_max =0.8), data = mean_migration_rates)

summary(tip.nls0.1)

m0.1.res1 <- nlsResiduals(tip.nls0.1)
plot(m0.1.res1, which = 0)

overview(tip.nls0.1)

plotfit(tip.nls0.1, smooth = TRUE)

tip.cont1 <- nlsContourRSS(tip.nls0.1)
plot(tip.cont1, col = FALSE, nlev = 5)

AIC(tip.nls0.1)
AIC(m0,  m2, tip.nls0, tip.nls0.1)


tip_dis_ker <- mean_migration_rates %>% 
  ggplot(aes(x = abs_diff, y = migration_rate)) + geom_point() + 
  geom_line(data = data.frame(abs_diff = mean_migration_rates$abs_diff, predi = predict(tip.nls0.1, type= 'response')),
            aes(x = abs_diff, y = predi))+
  ylab("Dispersal rate") + xlab("Distance (Km)")

tip_dis_ker_inset <- mean_migration_rates %>% 
  filter(migration_rate < 0.23) %>% 
  ggplot(aes(x = abs_diff, y = migration_rate)) + geom_point() + 
  geom_line(data = filter(data.frame(abs_diff = mean_migration_rates$abs_diff, predi = predict(tip.nls0.1, type= 'response')), abs_diff > 0),
            aes(x = abs_diff, y = predi))+ 
  ylab("") + xlab("") + scale_x_continuous(limits = c(0,26), breaks = seq(0,25,5)) +scale_y_continuous(limits = c(0, 0.06), breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06))+
  theme(axis.text = element_text(size = 30))

ggsave(tip_dis_ker, filename = "Figures/tip_dis_ker.jpeg")
ggsave(tip_dis_ker_inset, filename = "Figures/tip_dis_ker_inset.jpeg", width = 9.2, height = 7.6)

