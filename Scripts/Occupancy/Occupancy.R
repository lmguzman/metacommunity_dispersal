library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

env <-read.csv("Other_data/Environment.csv")
spe <- read.csv("Other_data/Species_presence.csv")

env %>% glimpse()

env$Actual_Vol %>% hist()

spe %>% head()

spe %>%
  select(Brom, Leptagrion, Tipulidae) %>% 
  gather(key = "species", value = "presence", -Brom) %>% 
  ggplot(aes(x = Brom, y = species, colour = as.factor(presence)))+ geom_point(size = 5, pch =15) 

spe %>%
  select(Leptagrion, Tipulidae) %>% 
  colSums()

spe %>%
  select(Brom, Leptagrion, Tipulidae) %>% 
  left_join(select(env, Brom, Max_Vol, Actual_Vol)) %>% 
  gather(key = "species", value = "presence", Leptagrion, Tipulidae) %>% 
  ggplot(aes(x = Actual_Vol, y = presence)) + geom_point() + facet_wrap(~species) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)



sel_spe <- spe %>%
  select(Brom, Leptagrion, Tipulidae) %>% 
  left_join(select(env, Brom, Max_Vol, Actual_Vol))

tip_glm <- glm(Tipulidae ~ Actual_Vol, family = binomial(link = 'logit'), data = sel_spe)

summary(tip_glm)

dam_glm <- glm(Leptagrion ~ Actual_Vol, family = binomial(link = 'logit'), data = sel_spe)

summary(dam_glm)



