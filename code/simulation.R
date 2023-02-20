betas <- rep(0,32)
betas[1] <- 2
betas <- betas[order(-betas)]
ids <- c(1:length(betas))
division <- rep(rep(1:4, each = 4),2)
conference <- rep(1:2, each = 16)

dat <- data.frame(ids, betas, division, conference)

library(combinat)
#Division games
div <- list()
for (i in 1:8){
  out <- t(combn((4*(i - 1)+1):(4*i), 2))
  rbind(out, cbind(out[,2],out[,1]))
div[[i]] <- out
}
one <- do.call(rbind,div)

#Cross division, intra-conference.  
cdic <- list()
#1 vs 2, 3 vs 4, 5 vs 6, 7 vs 8
cdic[[1]] <- expand.grid(1:4,5:8)
cdic[[2]] <- expand.grid(9:12,13:16)
cdic[[3]] <- expand.grid(17:20,21:24)
cdic[[4]] <- expand.grid(25:28,29:32)

two <- as.matrix(do.call(rbind,cdic))

#Cross Conference 
#1 vs 5, 2 vs 6, 3, vs 7, 4 vs 8
cc <- list()
#1 vs 2, 3 vs 4, 5 vs 6, 7 vs 8
cc[[1]] <- expand.grid(1:4,17:20)
cc[[2]] <- expand.grid(5:8,21:24)
cc[[3]] <- expand.grid(9:12,25:28)
cc[[4]] <- expand.grid(13:16,29:32)

three <- as.matrix(do.call(rbind,cc))

#Rank based games
four

#Need to add part 4
schedule <- rbind(one,two,three)
colnames(schedule) <- c("H","A")

#Simulate games 
alpha <- 0
results <- rep(0,nrow(schedule))
for (j in 1:nrow(schedule)){
  #Alpha is hfa
  xb <- betas[schedule[j,1]] - betas[schedule[j,2]] + alpha 
  p <- exp(xb) / (1+ exp(xb))
  results[j] <- sample(schedule[j,], 1, prob = c(p,1-p))
}

#Regular season results.  
dat$wins <- table(factor(results, levels = 1:32))

#Top 14 make playoffs 
library(tidyverse)
dat <- dat %>% arrange(-wins)
dat$seed <- 1:32
playoffs <- dat %>% filter(seed <=14)

playoff_results <- list()

bracket <- data.frame(H = c(1,8,4,5,6,3,7,2))
bracket$A <- 17  - bracket$H

#Takes in 2 seeds returns a simulated winner
#alpha is home field advantage
alpha <- 0
simplayoff <- function(x){
  if (x[2] > 14){return(x[1])}
  xb <- playoffs$beta[playoffs$seed == x[1]] - playoffs$beta[playoffs$seed == x[2]] + alpha
  p <- exp(xb) / (1+ exp(xb))
  return(sample(x, 1, prob = c(p,1-p)))
}

#Simulated playoffs
round1 <- apply(as.matrix(bracket),1,simplayoff)
greg <- matrix(round1,ncol = 2, byrow = TRUE)
round2 <- apply(greg,1,simplayoff)
steve <- matrix(round2,ncol = 2, byrow = TRUE)
round3 <- apply(steve,1,simplayoff)
champion <- simplayoff(round3)







