
library(stringr)
library(tidyr)
library(igraph)
library(dplyr)

###### Damselfly #######


inds <- read.table("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/bayesass_outputs/run_3/individuals.txt")


all_ancestries <- data.frame()

for(i in 1:4){
  
  migrant_ancests <- read.table(paste0("output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/bayesass_outputs/run_",i,"/migrant_ancestries.txt"))
  
  prob_mig <- apply(migrant_ancests, 2, FUN = function(x){as.numeric(str_extract(x, pattern = "\\d.\\d{3}"))})
  
  colnames(prob_mig) <- paste0("pop_",1:ncol(prob_mig))
  
  ancests <- data.frame(ind = rep(inds$V2, each = 3), pop = (rep(inds$V5, each = 3) +1), disp_event = rep(0:2, length(inds$V2)), prob_mig) %>% 
    gather(key = 'population_origin', value = 'probability', -ind, -pop, -disp_event) %>% 
    filter(probability !=0) %>% 
    mutate(rep = i)
  
  all_ancestries <- bind_rows(all_ancestries, ancests)
}



ancest <- all_ancestries %>%
  group_by(ind, pop, disp_event, population_origin) %>% 
  dplyr::summarise(mean_prob = mean(probability), support = n()) %>% 
  ungroup() %>% 
  group_by(ind) %>% 
  filter(support == max(support)) %>% 
  separate(population_origin, c("pop_name", "pop_origin")) %>% 
  dplyr::select(-pop_name)
  

an2 <- ancest 

edge_vector <- c()
for(i in 1:nrow(an2)){
  edge_v<- c(an2$pop_origin[i], an2$pop[i])
  edge_vector <- c(edge_vector, edge_v)
}

gr <- graph(directed = TRUE, edges = edge_vector)

l <- layout_with_fr(gr)

E(gr)$disp_event <- an2$disp_event

E(gr)$width <- 5

ecol <- rep('grey50', length(E(gr)$disp_event))

vcol <- 'black'

jpeg("Figures/all_dam_net.jpeg", width = 1000, height = 1000, quality = 300)
plot.igraph(gr, edge.color="grey50", vertex.color="ghostwhite", vertex.size = 40,
            vertex.frame.color="black", edge.curved=0.01, edge.arrow.size=1, vertex.label.color= vcol, 
            vertex.label.family = 'sans', vertex.label.cex = 4, layout = l, edge.arrow.width = 3,
            rescale = FALSE, ylim=c(108,113),xlim=c(114,119), asp = 0)
dev.off()


ecol <- ifelse(E(gr)$disp_event == 1, "mediumorchid4", 'white')

E(gr)

gr1 <- delete_edges(gr,E(gr)[E(gr)$disp_event != 1])

jpeg("Figures/all_dam_net_1.jpeg", width = 1000, height = 1000, quality = 300)
plot.igraph(gr1, edge.color= "mediumorchid4", vertex.color="ghostwhite", vertex.size = 40,
            vertex.frame.color="black", edge.curved=0.01, edge.arrow.size=1, vertex.label.color= vcol, 
            vertex.label.family = 'sans', vertex.label.cex = 4, layout = l, edge.arrow.width = 3,
            rescale = FALSE, ylim=c(108,113),xlim=c(114,119), asp = 0)
dev.off()



gr2 <- delete_edges(gr,E(gr)[E(gr)$disp_event != 2])

jpeg("Figures/all_dam_net_2.jpeg", width = 1000, height = 1000, quality = 300)
plot.igraph(gr2, edge.color= "seagreen", vertex.color="ghostwhite", vertex.size = 40,
            vertex.frame.color="black", edge.curved=0.01, edge.arrow.size=1, vertex.label.color= vcol, 
            vertex.label.family = 'sans', vertex.label.cex = 4, layout = l, edge.arrow.width = 3,
            rescale = FALSE, ylim=c(108,113),xlim=c(114,119), asp = 0)
dev.off()


##### Tipulid ######


inds <- read.table("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/bayesass_outputs/run_3/individuals.txt")


all_ancestries <- data.frame()

for(i in 1:3){
  
  migrant_ancests <- read.table(paste0("output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/bayesass_outputs/run_",i,"/migrant_ancestries.txt"))
  
  prob_mig <- apply(migrant_ancests, 2, FUN = function(x){as.numeric(str_extract(x, patter = "\\d.\\d{3}"))})
  
  colnames(prob_mig) <- paste0("pop_",1:ncol(prob_mig))
  
  ancests <- data.frame(ind = rep(inds$V2, each = 3), pop = (rep(inds$V5, each = 3) +1), disp_event = rep(0:2, length(inds$V2)), prob_mig) %>% 
    gather(key = 'population_origin', value = 'probability', -ind, -pop, -disp_event) %>% 
    filter(probability !=0) %>% 
    mutate(rep = i)
  
  all_ancestries <- bind_rows(all_ancestries, ancests)
}

all_ancestries

ancest <- all_ancestries %>%
  group_by(ind, pop, disp_event, population_origin) %>% 
  dplyr::summarise(mean_prob = mean(probability), support = n()) %>% 
  ungroup() %>% 
  group_by(ind) %>% 
  filter(support == max(support)) %>% 
  separate(population_origin, c("pop_name", "pop_origin")) %>% 
  dplyr::select(-pop_name)


##### Figure

an2 <- ancest 

edge_vector <- c()
for(i in 1:nrow(an2)){
  edge_v<- c(an2$pop_origin[i], an2$pop[i])
  edge_vector <- c(edge_vector, edge_v)
}

gr <- graph(directed = TRUE, edges = edge_vector)

l <- layout_with_fr(gr)

E(gr)$disp_event <- an2$disp_event

E(gr)$width <- 5

ecol <- rep('grey50', length(E(gr)$disp_event))

vcol <- 'black'

jpeg("Figures/all_tip_net.jpeg", width = 1000, height = 1000, quality = 300)
plot.igraph(gr, edge.color="grey50", vertex.color="ghostwhite", vertex.size = 40,
            vertex.frame.color="black", edge.curved=0.01, edge.arrow.size=1, vertex.label.color= vcol, 
            vertex.label.family = 'sans', vertex.label.cex = 4, layout = l, edge.arrow.width = 3,
            rescale = FALSE, ylim=c(7,12),xlim=c(10,14), asp = 0)
dev.off()


ecol <- ifelse(E(gr)$disp_event == 1, "mediumorchid4", 'white')

E(gr)

gr1 <- delete_edges(gr,E(gr)[E(gr)$disp_event != 1])

jpeg("Figures/all_tip_net_1.jpeg", width = 1000, height = 1000, quality = 300)
plot.igraph(gr1, edge.color= "mediumorchid4", vertex.color="ghostwhite", vertex.size = 40,
            vertex.frame.color="black", edge.curved=0.01, edge.arrow.size=1, vertex.label.color= vcol, 
            vertex.label.family = 'sans', vertex.label.cex = 4, layout = l, edge.arrow.width = 3,
            rescale = FALSE, ylim=c(7,12),xlim=c(10,14), asp = 0)
dev.off()



gr2 <- delete_edges(gr,E(gr)[E(gr)$disp_event != 2])

jpeg("Figures/all_tip_net_2.jpeg", width = 1000, height = 1000, quality = 300)
plot.igraph(gr2, edge.color= "seagreen", vertex.color="ghostwhite", vertex.size = 40,
            vertex.frame.color="black", edge.curved=0.01, edge.arrow.size=1, vertex.label.color= vcol, 
            vertex.label.family = 'sans', vertex.label.cex = 4, layout = l, edge.arrow.width = 3,
            rescale = FALSE, ylim=c(7,12),xlim=c(10,14), asp = 0)
dev.off()
