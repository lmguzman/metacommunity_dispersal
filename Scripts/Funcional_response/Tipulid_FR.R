###### functional response for the tipulid####

library(dplyr)
library(deSolve)
library(bbmle)

Feeding <- read.csv('Other_data/FeedingTrial.csv')

ControlFeeding <- Feeding %>%
  dplyr::select(-X) %>%
  dplyr::filter(TreatmentType == 'Control') %>% 
  filter(Prey == 'Tipulid')


#Type two functional response

type_two <- function(param) {
  N <- seq(0,25,1)
  Ne <- (param[1] * N)/(1+ param[1] * param[2] * N)
  return(Ne)
}


### Functions required for maximum likelihood

attack <- function(b,N) {
  b*N
}
### may be wrong, since its multiplying N with A = b*N (so in this case it would N2)
gradfun <- function(t,y,parms) {
  with(as.list(c(y,parms)),
       { A  <- attack(b,N)
       grad <- -A*N/(1+A*h*N)
       list(grad,NULL)
       })
}

gradfun <- function(t,y,parms) {
  with(as.list(c(y,parms)),
       { A  <- attack(b,N)
       grad <- -A/(1+A*h)
       list(grad,NULL)
       })
}

fun2 <- function(b,h,T, P,N0) {
  library(deSolve)
  L <- lsoda(c(N=N0),
             times=c(0,T),
             func=gradfun,
             parms=c(b=b,h=h,P=P))
  N.final <- L[2,2]
  N0-N.final
}

NLL.oneT = function(b, h, T = 4, P = 1) {
  b <- b
  h <- h
  prop.exp <- numeric(length=length(Initial))
  prop.exp <- sapply(Initial, fun2,
                     b=b, h=h, P=P, T=T)/Initial
  - sum(dbinom(Killed, prob = prop.exp, size = Initial,
               log = TRUE))
}


OneTreatmentModel <- function(offered, eaten, starts){
  
  #packages required
  library(bbmle)
  
  #Making sure data has at least 2 columns
  #if(nrow(offered) != nrow(eaten)) stop ("Prey offered and eaten dont have the same lenght")
  
  #We are organizing the data
  dd.ml = list(Initial=offered,
               Killed=eaten)
  
  # we set up some plausible starting values with upper and lower limits
  #start.vals = list(starts)
  # lower.lims = 
  #upper.lims = 
  
  #Using maximum likelyhood and NLL.oneT
  full.mod <- mle2(NLL.oneT, start=starts, data = dd.ml,
                   method="L-BFGS-B",
                   lower=c(b = 1e-10, h = 1e-10),
                   upper=c(b = 3, h = 8))
  return(list(summary(full.mod), full.mod))
}


model_output <- OneTreatmentModel(offered = ControlFeeding$PreyDensity, eaten = ControlFeeding$NoPreyEaten, starts = list(b = 0.2, h = 0.05))


AIC(model_output[[2]])


a <- 0.1700837

h <- 1.1698634

x <- seq(0, 20, by=0.1)

par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,7,3,2))

plot(1, type = 'n', xlab = 'Prey Density', ylab = 'Prey Eaten', ylim = c(0, 10), xlim = c(0,20))


curve(sapply(x, fun2, b=a,
             h=h,
             T=4, P=1),
      add=TRUE, lty=1,lwd=3, from=0, col='blue')
points(x = ControlFeeding$PreyDensity, y = ControlFeeding$NoPreyEaten, ylab = "Number of prey offered")











#Type three functional response



attack <- function(b,q,N) {
  b*N^q
}

gradfun <- function(t,y,parms) {
  with(as.list(c(y,parms)),
       { A  <- attack(b,q,N)
       grad <- -A*N/(1+A*h*N)
       list(grad,NULL)
       })
}

fun2 <- function(b,q,h,P,T,N0) {
  L <- lsoda(c(N=N0),
             times=c(0,T),
             func=gradfun,
             parms=c(b=b,q=q,h=h,P=P))
  N.final <- L[2,2]
  N0-N.final
}


NLL.oneT = function(b,q, h, T = 4, P = 1) {
  b <- b
  h <- h
  q <- q
  prop.exp <- numeric(length=length(Initial))
  prop.exp <- sapply(Initial, fun2,
                     b=b, h=h,q=q, P=P, T=T)/Initial
  - sum(dbinom(Killed, prob = prop.exp, size = Initial,
               log = TRUE))
}


OneTreatmentModel <- function(offered, eaten, starts){
  
  #packages required
  library(bbmle)
  
  #Making sure data has at least 2 columns
  #if(nrow(offered) != nrow(eaten)) stop ("Prey offered and eaten dont have the same lenght")
  
  #We are organizing the data
  dd.ml = list(Initial=offered,
               Killed=eaten)
  
  # we set up some plausible starting values with upper and lower limits
  #start.vals = list(starts)
  # lower.lims = 
  #upper.lims = 
  
  #Using maximum likelyhood and NLL.oneT
  full.mod <- mle2(NLL.oneT, start=starts, data = dd.ml,
                   method="L-BFGS-B",
                   lower=c(b = 1e-10, h = 1e-10, q = 1e-10),
                   upper=c(b = 3, h = 8, q = 10))
  return(list(summary(full.mod), full.mod))
}


model_output <- OneTreatmentModel(offered = ControlFeeding$PreyDensity, eaten = ControlFeeding$NoPreyEaten, starts = list(b = 0.2, h = 0.05, q = 1))

AIC(model_output[[2]])


a <- coef(model_output)[1,1]

h <- coef(model_output)[2,1]

x <- seq(0, 20, by=0.1)

par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,7,3,2))

plot(1, type = 'n', xlab = 'Prey Density', ylab = 'Prey Eaten', ylim = c(0, 10), xlim = c(0,20))

curve(sapply(x, fun2, b=a,
             h=h,
             T=4, P=1),
      add=TRUE, lty=1,lwd=3, from=0, col='blue')
points(x = ControlFeeding$PreyDensity, y = ControlFeeding$NoPreyEaten)

