library(dplyr)
library(tidyr)
library(parallel)

prey_advance2 <- function(pret, rv, rp, kv, kp, av, hv, pred, B){
  pret2 <- exp(rv*(1 - (pret/kv)) - av*pred/(1 + av*hv*pret))
  pred2 <- exp(rv*(1 - (pred/kp)) - B*av*pret/(1 + av*hv*pret))
  nt2 <- cbind(t(pret2), t(pred2))
  return(nt2)
}

s1 <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0)
s3 <- c(0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)


run_model <- function(n, df_sup){
  
  params <- unlist(df_sup[n,])
  names(params) <- NULL
  
  generations <- 2000 
  days_per_gen_tip <- 60
  days_per_gen_dam <- 270
  x <- 5
  y <- 5
  
  patches <- x*y
  
  distance_all <- as.matrix(read.csv("distance_all.csv"))
  
  ### dispersal rate 
  
  d_fin <- distance_all*100 
  ### dispersal rate 
  
  kernel_tipulid <- 0.03498*exp(-0.02964 * d_fin)
  diag(kernel_tipulid) <- 0
  mig_max_t <- 0.80714
  row_t_t <- 1-mig_max_t
  row_t_t_day <- row_t_t/days_per_gen_tip
  #scal <- rowSums(kernel_tipulid)/row_t_t_day
  #dispersal_tipulid <- kernel_tipulid/scal
  scal <- row_t_t_day/rowSums(kernel_tipulid)
  dispersal_tipulid <- kernel_tipulid*scal
  diag(dispersal_tipulid) <- (1-row_t_t_day)
  rowSums(dispersal_tipulid)
  
  
  kernel_damsel <- 0.020323*exp(-0.002985 * d_fin)
  #dispersal_damsel <- 0*distance_all
  diag(kernel_damsel) <- 0
  mig_max_d <- 0.850162
  row_t_d <- 1-mig_max_d
  row_t_d_day <-  row_t_d/days_per_gen_dam
  #scald <- rowSums(kernel_damsel)/row_t_d_day
  #dispersal_damsel <- kernel_damsel/scald
  scald <- row_t_d_day/rowSums(kernel_damsel)
  dispersal_damsel <- kernel_damsel*scald
  diag(dispersal_damsel) <- (1-row_t_d_day)
  rowSums(dispersal_damsel)
  
  
  rv <- params[1]
  kv <- params[2]
  rp <- params[4]
  kp <- params[5]
  av <- c(0.1700837)
  hv <- c(1.1698634)
  
  B <- params[3]
  R0 <- 10
  P0 <- 7
  
  
  s1[s1 > 0] <- R0
  pret <- s1
  names(pret) <- paste0("X", 1:patches)
  
  s3[s3 > 0] <- P0
  pred <- s3
  names(pred) <- paste0("X", 1:patches)
  
  abundances_prey <- data.frame(time = 0, t(pret))
  abundances_pred <- data.frame(time = 0, t(pred))
  
  for(g in 1:generations){
    
    #N_j(t+1)=D_{N_o}*N_j(t) * exp(r*[1-N_j(t)/K]-a P_j(t)/(1+a*h*N_j(t)) +dispersal term from other patches
    
    nt2 <- prey_advance2(pret, rv, rp, kv, kp, av, hv, pred, B)
    
    pret <- pret*nt2[1:patches, drop = FALSE]
    pred <- pred*nt2[(patches + 1):(patches*2)]
    
    ## dispersal
    
    pret2 <- pret %*% dispersal_tipulid
    pred2 <- pred %*% dispersal_damsel
    
    pret2[pret2 < 0.001] <- 0
    pred2[pred2 < 0.001] <- 0
    
    
    pret <- pret2
    pred <- pred2
    
    abundances_prey <- rbind(abundances_prey, data.frame(time = g, pret))
    abundances_pred <- rbind(abundances_pred, data.frame(time = g, pred))
  }
  
   saveRDS(list(prey = abundances_prey, pred = abundances_pred), file = 
             paste0("/home/lmguzman/scratch/sequencing_model/torus_no_c/scenario_data/r_",params[1],"_K_",params[2],"_B_",params[3],"_rp_",params[4],"_kp_",params[5],".RDS"))
   
  pred_f <- abundances_pred%>% 
    filter(time == generations) %>% 
    dplyr::select(starts_with("X"))
  
  prey_f <- abundances_prey%>%
    filter(time == generations) %>%
    dplyr::select(starts_with("X"))
  
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
  
  r5 <- am_prey[am_prey$time_chuncks == 15,4] > am_prey[am_prey$time_chuncks == 5,4]
  
  
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
  return(list(r1, r2, r3,r4, r5,params, cs))
  
}

rs <- seq(0.01, 3, 0.4)
Ks <- seq(2, 40, 5)
B <- seq(0.1, 1, 0.1)
rp <- seq(0.001, 0.5, 0.01)
kp <- seq(1, 11, 2)
df_all <- expand.grid(rs, Ks, B,rp, kp)

testFunction <- function(n, df_sup) {
  return(tryCatch(run_model(n, df_sup), error=function(e) NULL))
}

pred_per <- mclapply(1:nrow(df_all), FUN = testFunction, mc.cores = 48, df_all)

saveRDS(pred_per, "/home/lmguzman/scratch/sequencing_model/torus_no_c/data_v2.rds")
