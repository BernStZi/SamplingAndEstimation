#################################
## Setting the Directory ########
#################################

path <- "F:/Sand_Summerschool/Excersises/"
setwd(path)
library(sampling)
data("belgianmunicipalities")
bm <- belgianmunicipalities

#########################
## Excersise 3###########
#########################

##################
###Functions######
##################

###Varince under StrRS
GetStratVar <- function(Y, sind, nh) {
  Nh <- tapply(sind,sind,length)
  N <- length(sind)
  sum(Nh^2*tapply(Y,sind,function(x) 
    var(x)*(length(x)-1)/length(x))/nh)/N^2
}
###Calculating nh,prop
GetOptAlloc <- function(Y, sind, n){
  L <- length(unique(sind))
  nh <- rep(2,L)
  Nh <- tapply(sind,sind,length)
  v <- numeric(L)
  M <- diag(rep(1,L))
  while (sum(nh) < n) {
    for (i in 1:L) {
      if (nh[i] == Nh[i]) {
        v[i] <- Inf
      } else {
        v[i] <- GetStratVar(Y, sind, nh + M[,i])
      }
    }
    nh <- nh + M[,which.min(v)]
  }
  nh
}
##########################
#####Parameters###########
##########################

gamma <- prop.table(table(bm$Province)) #prop in population
n <- 180 #srswor
nh.eq <- rep(180/length(unique(bm$Province)),9) #equal allocation
nh.prop1 <- round(gamma*n) #proportional allocation
nh.prop2 <- round(tapply(bm$averageincome,bm$Province,sum)/
                    sum(bm$averageincome)*n)
nh.prop3 <- round(tapply(bm$TaxableIncome,bm$Province,sum)/
                    sum(bm$TaxableIncome)*n)


cor(bm$averageincome,bm$Tot04)
# -0.06104849
cor(bm$Tot04,bm$TaxableIncome)
# 0.9882652
nh.opt <- round(GetOptAlloc(bm$Tot04,bm$Province,n)) #optimal all. Tot04




l1 <- 2 #SRCS 1
l2 <- 6 #SRCS 2

R <- 1000 #Number of simulations

##########################
#### Functions 2 #########
##########################

####SRSWOR
SRS.mean <- function(Y,S){return(mean(Y[S]))}
fpc <- function(N,n){(N-n)/N}
SRSWOR.evar <- function(Y,S)
{return(fpc(length(Y),length(S))*var(Y[S])/length(S))}
####StrRS
Strat.mean <- function(Y,sind,S){
  Nh <- tapply(Y,sind,length)
  Str.mean <- sum(Nh*tapply(Y[S], sind[S], mean) / sum(Nh))
  return(Str.mean)
}

Strat.evar<- function(Y, sind, S) {
  Nh <- tapply(sind,sind,length)
  nh <- tapply(sind[S], sind[S], length)
  ssh <- tapply(Y[S], sind[S], var)
  res <- sum((Nh/sum(Nh))^2*ssh/nh*(Nh-nh)/Nh)
  return(res)
}

####SCRS
SCRS.mean <-function(Y,sind,S){
  L <- length(unique(sind))
  l <- length(unique(sind[S]))
  N <- length(Y)
  N_h_a <- tapply(Y[S],sind[S],length)
  mu_h_a <- tapply(Y[S],sind[S],mean)
  return(L/l*sum(N_h_a/N*mu_h_a))
}

se.sq <- function(Y,sind,S){
  L <- length(unique(sind))
  l <- length(unique(sind[S]))
  N <- length(Y)
  mu.SCRS <- SCRS.mean(Y,sind,S)
  c <- N*mu.SCRS/L
  mu_h_a <- tapply(Y[S],sind[S],mean)
  N_h_a <- tapply(Y[S],sind[S],length)
  return ( 1/(l-1)*sum((N_h_a*mu_h_a-c)^2))
}

SCRS.evar <- function(Y,sind,S){
  L <- length(unique(sind))
  l <- length(unique(sind[S]))
  N <- length(Y)
  part1 <- L^2/N^2
  part2 <- se.sq(Y,sind,S)/l
  part3 <- (L-l)/L
  return(part1*part2*part3)
}
########################
#### New Functions######
########################

####StrRS,eq###########
Strat.eq.evar <- function(Y,sind,S){
  L <- length(unique(sind))
  Nh <- tapply(sind,sind,length)
  nh <- tapply(sind[S], sind[S], length)
  ssh <- tapply(Y[S], sind[S], var)
  res <- 1/sum(nh)*sum((Nh/sum(Nh))^2*ssh*
                         ((Nh*L-sum(nh))/(Nh-1)))
  return(res)
}


#####StrRS,opt
Strat.opt.evar <- function(Y, sind, S){
  Nh <- tapply(sind,sind,length)
  nh <- tapply(sind[S], sind[S], length)
  ssh <- tapply(Y[S], sind[S], var)
  part1 <- 1/sum(nh)*sum(Nh/sum(Nh)*sqrt(ssh))^2
  part2 <- 1/sum(Nh)*sum(Nh/sum(Nh)*ssh)
  return(part1-part2)
}



#############################
#### Simulation #############
#############################


####SRS###

SRS.par <- matrix(nrow = R, ncol = 2)
colnames((SRS.par)) <- c("Mean","Var")

for(i in 1:R){
  s <- srswor(n,nrow(bm))
  samp <- bm[s == 1,]
  S <- as.numeric(rownames(samp))
  SRS.par[i,1] <- SRS.mean(bm$Tot04,S)
  SRS.par[i,2] <- SRSWOR.evar(bm$Tot04,S)
  cat("Step",i,"\n")
}

###StrRS,eq##########
StrRS.eq.par <-  matrix(nrow = R, ncol = 2)
colnames((StrRS.eq.par)) <- c("Mean","Var")

for(i in 1:R){
  s <- strata(bm,"Province",nh.eq, method = "srswor")
  samp <- getdata(bm,s)
  S <- as.numeric(rownames(samp))
  StrRS.eq.par[i,1] <- Strat.mean(bm$Tot04,bm$Province,S)
  StrRS.eq.par[i,2] <- Strat.eq.evar(bm$Tot04,bm$Province,S)
  cat("Step",i,"\n")
}

###StrRS,prop 1##########
StrRS.prop1.par <-  matrix(nrow = R, ncol = 2)
colnames(StrRS.prop1.par) <- c("Mean","Var")


for(i in 1:R){
  s <- strata(bm,"Province",nh.prop1, method = "srswor")
  samp <- getdata(bm,s)
  S <- as.numeric(rownames(samp))
  StrRS.prop1.par[i,1] <- Strat.mean(bm$Tot04,bm$Province,S)
  StrRS.prop1.par[i,2] <- Strat.evar(bm$Tot04,bm$Province,S)
  cat("Step",i,"\n")
}

###StrRS,prop 2##########
StrRS.prop2.par <-  matrix(nrow = R, ncol = 2)
colnames(StrRS.prop2.par) <- c("Mean","Var")


for(i in 1:R){
  s <- strata(bm,"Province",nh.prop2, method = "srswor")
  samp <- getdata(bm,s)
  S <- as.numeric(rownames(samp))
  StrRS.prop2.par[i,1] <- Strat.mean(bm$Tot04,bm$Province,S)
  StrRS.prop2.par[i,2] <- Strat.evar(bm$Tot04,bm$Province,S)
  cat("Step",i,"\n")
}

###StrRS,prop 3##########
StrRS.prop3.par <-  matrix(nrow = R, ncol = 2)
colnames(StrRS.prop3.par) <- c("Mean","Var")



for(i in 1:R){
  s <- strata(bm,"Province",nh.prop3, method = "srswor")
  samp <- getdata(bm,s)
  S <- as.numeric(rownames(samp))
  StrRS.prop3.par[i,1] <- Strat.mean(bm$Tot04,bm$Province,S)
  StrRS.prop3.par[i,2] <- Strat.evar(bm$Tot04,bm$Province,S)
  cat("Step",i,"\n")
}


###StrRS,opt ##########
StrRS.opt.par <-  matrix(nrow = R, ncol = 2)
colnames(StrRS.opt.par) <- c("Mean","Var")


for(i in 1:R){
  s <- strata(bm,"Province",nh.opt, method = "srswor")
  samp <- getdata(bm,s)
  S <- as.numeric(rownames(samp))
  StrRS.opt.par[i,1] <- Strat.mean(bm$Tot04,bm$Province,S)
  StrRS.opt.par[i,2] <- Strat.opt.evar(bm$Tot04,bm$Province,S)
  cat("Step",i,"\n")
}

####SRCS 1##############
SRCS1.par <-  matrix(nrow = R, ncol = 2)
colnames(SRCS1.par) <- c("Mean","Var")

for(i in 1:R){
  s <- cluster(bm,"Province",l1, method = "srswor")
  samp <- getdata(bm,s)
  S <- as.numeric(rownames(samp))
  SRCS1.par[i,1] <- SCRS.mean(bm$Tot04,bm$Province,S)
  SRCS1.par[i,2] <- SCRS.evar(bm$Tot04,bm$Province,S)
  cat("Step",i,"\n")
}

####SRCS 2##############
SRCS2.par <-  matrix(nrow = R, ncol = 2)
colnames(SRCS2.par) <- c("Mean","Var")

for(i in 1:R){
  s <- cluster(bm,"Province",l2, method = "srswor")
  samp <- getdata(bm,s)
  S <- as.numeric(rownames(samp))
  SRCS2.par[i,1] <- SCRS.mean(bm$Tot04,bm$Province,S)
  SRCS2.par[i,2] <- SCRS.evar(bm$Tot04,bm$Province,S)
  cat("Step",i,"\n")
}

################################
###### average mean and var ####
################################

SIM <- rbind("SRS.sim" = apply(SRS.par,2,mean),
"StrRS.eq.sim" = apply(StrRS.eq.par,2,mean),
"StrRS.prop1.sim" = apply(StrRS.prop1.par,2,mean),
"StrRS.prop2.sim" = apply(StrRS.prop2.par,2,mean),
"StrRS.prop3.sim" = apply(StrRS.prop3.par,2,mean),
"StrRS.opt.sim" = apply(StrRS.opt.par,2,mean),
"SRCS1.sim" = apply(SRCS1.par,2,mean),
"SRCS2.sim" = apply(SRCS2.par,2,mean))

mean(bm$Tot04)
# 17686.12
 
sort(SIM[,1]-mean(bm$Tot04))
sort(SIM[,2])

########## Plot ###########
Means <- data.frame(SRS = SRS.par[,1], Str.eq =StrRS.eq.par[,1],
                    Str.opt = StrRS.opt.par[,1], 
                    Str.prop1 = StrRS.prop1.par[,1],
                    Str.prop2 = StrRS.prop2.par[,1],
                    Str.prop3 = StrRS.prop3.par[,1],
                    Clus1 = SRCS1.par[,1],
                    Clus2 = SRCS2.par[,1])


boxplot(Means,col = c("thistle","wheat","turquoise",
                     "tomato","red","yellow","blue",
                     "sienna"))
abline(h=mean(bm$Tot04))




