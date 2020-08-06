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
  
  kernel_tipulid <- params[12]*exp(-params[13] * d_fin)
  diag(kernel_tipulid) <- 0
  mig_max_t <- params[11]
  row_t_t <- 1-mig_max_t
  row_t_t_day <- row_t_t/days_per_gen_tip
  #scal <- rowSums(kernel_tipulid)/row_t_t_day
  #dispersal_tipulid <- kernel_tipulid/scal
  scal <- row_t_t_day/rowSums(kernel_tipulid)
  dispersal_tipulid <- kernel_tipulid*scal
  diag(dispersal_tipulid) <- (1-row_t_t_day)
  rowSums(dispersal_tipulid)
  
  kernel_damsel <- params[9]*exp(-params[10] * d_fin)
  #dispersal_damsel <- 0*distance_all
  diag(kernel_damsel) <- 0
  mig_max_d <- params[8]
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
  av <- params[6]
  hv <- params[7]
  
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
	paste0("/home/lmguzman/scratch/sequencing_model/torus_random/scenario_data/r_",params[1],"_K_",params[2],"_B_",params[3],"_rp_",
	params[4],"_kp_",params[5],"_a_", params[6], "_h_", params[7], "_Dp0_", params[8], "_Dp1_", params[9],"_bp_",params[10],
	"_Dn0_", params[11], "_Dn1_", params[12],"_bn_", params[13], "_.RDS"))
  
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


rs <- c(0.41, 1.61, 2.81)
Ks <- c(7, 22, 37)
B <- c(0.2, 0.5, 0.8)
rp <- c(0.001, 0.181, 0.361)
kp <- c(1, 5, 11)

df_all <- expand.grid(rs, Ks, B, rp, kp)

set.seed(10)
a <- runif(50,0.170 - 0.081, 0.170 + 0.081)
h <- runif(50, 1.170 -0.604, 1.170 +0.604)
Dp0 <- runif (50, 0.850 -0.005, 0.850 +0.005)
Dp1 <-  runif(50, 0.020 -0.003, 0.020 +0.003)
bp <- runif(50, 0, 0.003 +0.013)
Dn0 <- runif(50, 0.807 -0.010, 0.807+0.010)
Dn1 <- runif(50, 0.035 -0.007, 0.035 +0.007)
bn <- runif(50, 0.030 -0.022, 0.030 +0.022)

new_com <- expand.grid(1:nrow(df_all), 1:50)

df_all <- rbind(cbind(df_all[new_com$Var1,], a = a[new_com$Var2], h = 1.170, Dp0 = 0.850, Dp1 = 0.020, bp = 0.003,Dn0=  0.807, Dn1 = 0.035, bn = 0.030),
      cbind(df_all[new_com$Var1,], a = 0.170, h = h[new_com$Var2], Dp0 = 0.850, Dp1 =0.020, bp = 0.003, Dn0= 0.807, Dn1 = 0.035, bn = 0.030),
      cbind(df_all[new_com$Var1,], a = 0.170,h = 1.170,  Dp0 = Dp0[new_com$Var2], Dp1 =0.020, bp = 0.003, Dn0= 0.807, Dn1 = 0.035,bn =  0.030),
      cbind(df_all[new_com$Var1,], a = 0.170,h = 1.170, Dp0 = 0.850, Dp1 =Dp1[new_com$Var2], bp = 0.003, Dn0= 0.807,Dn1 =  0.035, bn =  0.030),
      cbind(df_all[new_com$Var1,], a = 0.170,h = 1.170, Dp0 = 0.850,Dp1 =0.020, bp = bp[new_com$Var2], Dn0= 0.807,Dn1 =  0.035,bn =  0.030),
      cbind(df_all[new_com$Var1,], a = 0.170,h = 1.170, Dp0 = 0.850,Dp1 =0.020, bp = 0.003,Dn0= Dn0[new_com$Var2],Dn1 =  0.035,bn =  0.030),
      cbind(df_all[new_com$Var1,], a = 0.170,h = 1.170, Dp0 = 0.850,Dp1 =0.020, bp = 0.003,Dn0= 0.807, Dn1 = Dn1[new_com$Var2],bn =  0.030),
      cbind(df_all[new_com$Var1,], a =0.170,h = 1.170, Dp0 = 0.850,Dp1 =0.020, bp = 0.003,Dn0= 0.807, Dn1 =  0.035, bn = bn[new_com$Var2]))


testFunction <- function(n, df_sup) {
  return(tryCatch(run_model(n, df_sup), error=function(e) NULL))
}

pred_per <- mclapply(1:nrow(df_all), FUN = testFunction, mc.cores = 80, df_all)

saveRDS(pred_per, "/home/lmguzman/scratch/sequencing_model/torus_random/data_v2.rds")

