#options(set.seed(42)) #### setting a global option

###############################
######### Getting Started #####
###############################
path <- "F:/Sand_Summerschool/"
setwd(path)



#######################
##### Subsetting ######
#######################

#### Slide 2 ##########


a <- as.vector(c("Aa","AA","Aa","Bb",
                 "AA","A","BB","Ba"))
a=="Aa"
b <- a[a=="Aa"]
b <- a[a!="AA"]


#### Slide 3 ###########

b <- 1:length(a)
a == "AA" & b > 3
ab <- which(a=="Aa" & b<=3)
ab
a[ab]

# Does an element belong to a group?

a %in% c("AA","Ba")

### Slide 4 ############

library(sampling)  # Usually at the beginning of your script
data("belgianmunicipalities")
bm<-belgianmunicipalities

sub1 <- bm[bm$Province==3,]
head(sub1[,1:7])
s <- which(bm$Commune %in% c("Brecht", "Grimbergen","As","Dinant"))
sub2 <- bm[s,]
sub2[,1:7]

### Slide 6 ############
?subset
sb <- subset(bm$Tot04,bm$Province>3)

sub3 <- subset(bm,Commune %in% c("Brecht", "Grimbergen","As","Dinant"))
sub3$Commune == sub2$Commune

sub4 <- subset(bm,substr(as.character(Commune),1,1)=="B")
head(sub4[,1:7])


#########################
### Loops ###############
#########################

### Slide 7 #############
a <- sample(1:10,10)


A1 <- vector()
for(i in 1:10){
  A1[i] <- sample(1:10,1)
}
A1

### Slide 8 #############


A2 <- matrix(nrow = 5,ncol = 2)
for(i in 1:nrow(A2)){
  A <- sample(1:50,30)
  A2[i,1] <- mean(A)
  A2[i,2] <- var(A)
}
A2

### Slide 9 #############
library(sampling)
data(belgianmunicipalities)
pik <- inclusionprobabilities(belgianmunicipalities$Tot04,200)
# Computes the inclusion probabilities
N <- length(pik)
# population size
n <- sum(pik)
# sample size
sim <- 1000
ss <- array(0, c(sim, 5))
# sim2 <- 10000   #second simulation
# ss2 <- array(0, c(sim2, 5))
# number of simulations
y <- belgianmunicipalities$TaxableIncome
# variable of interest
ht <- numeric(5)
# Horvitz-Thompson estimator for the simulation

### Slide 10a ########### 

for (i in 1:sim) {
  cat("Step ", i, "\n")
  s <- UPpoisson(pik)
  ht[1] <- HTestimator(y[s == 1], pik[s == 1])
  s <- UPrandomsystematic(pik)
  ht[2] <- HTestimator(y[s == 1], pik[s == 1])
  s <- UPsystematic(pik)
  ht[3] <- HTestimator(y[s == 1], pik[s == 1])
  s <- sample(y, n,replace = T)
  ht[4] <- HTestimator(s, rep(n/N,n))
  s <- srswor(n, N)
  ht[5] <- HTestimator(y[s == 1], rep(n/N, n))
  ss[i, ] <- ss[i, ] + ht 
} 

colnames(ss) <- c("Poisson","Rsyst","Syst","SRS","SRSWOR")

### Slide 10b ##########

 sim2 <- 10000   #second simulation
 ss2 <- array(0, c(sim2, 5))

 
 for (i in 1:sim2) {
   #cat("Step",i,"\n")
   s <- UPpoisson(pik)
   ht[1] <- HTestimator(y[s == 1], pik[s == 1])
   s <- UPrandomsystematic(pik)
   ht[2] <- HTestimator(y[s == 1], pik[s == 1])
   s <- UPsystematic(pik)
   ht[3] <- HTestimator(y[s == 1], pik[s == 1])
   s <- sample(y, n,replace = T)
   ht[4] <- HTestimator(s, rep(n/N,n))
   s <- srswor(n, N)
   ht[5] <- HTestimator(y[s == 1], rep(n/N, n))
   ss2[i, ] <- ss2[i, ] + ht 
   if(i%%100==0){
     cat("Step ", i, "\n")
   }
 } 
 colnames(ss2) <- c("Poisson","Rsyst","Syst","SRS","SRSWOR")
 
### Slide 11 ###########
par(mfrow=c(1,2))
boxplot(data.frame(ss), las = 3) 
boxplot(data.frame(ss2), las = 3)


### Slide 12 ###########
?apply


### Slide 13 ###########


Applydat <- matrix(1:25, nrow = 5, ncol = 5, byrow = F)
apply(Applydat,1,mean)
apply(Applydat,2,mean)

apply(Applydat,1,function(x)mean(x))

### Slide 14 ###########
?tapply

### Slide 15 ###########

Tapplydat <- data.frame(Income = rnorm(6,1400,200),
                        Gender = sample(c("Male","Female"),6,replace = T))
Tapplydat
tapply(Tapplydat$Income, Tapplydat$Gender, mean)


#######################
### Functions #########
#######################

### Slide 16 ##########

fpc <- function(x,y){
  N <- nrow(x)
  n <- length(y)
  f <- (N-n)/N
  return(f)
}

s <- srswor(100, nrow(bm))

fpc(bm,s[s==1])

### Slide 17 ##########
### Example CATI Samples

fra <-data.frame(pre = sample(c(30,40,89,221,621),
                              10000,replace = T),
                 bank = sample(100:99999,
                               10000,replace = T))
fra[1:4,]
fra <- fra[order(fra[,1]),]
fra[1:4,]

order(fra[,1])
sort(fra[1:10,1])
### Slide 18 ##########

tel.samp <- function(fra,n){
  len <- nrow(fra)*100
  s <- sort(sample(len,n))
  row <- ceiling(s/100)
  app <- s%%100
  ts <- fra[row,]
  num <- data.frame(prefix =
                      paste("0",ts[,1],sep = ""),
                    number =
                      paste(ts[,2],app,sep = ""))
  return(num)
}

### Slide 19 #########

my.first.ts <- tel.samp(fra,10)

head(my.first.ts)


sort(my.first.ts[,1])
order(my.first.ts[,1])

##########################
### Sampling with loops ##
##########################

### Slide 20 ##########

str.bm <- split(bm,bm$Province)
nh <- c(2,3,7,3,2,6,7,2,9)
res <- list()
for(i in 1:length(str.bm)){
  ID <- str.bm[[i]]$INS
  res[[i]] <- sample(ID,nh[i],replace=F)
}
s <- unlist(res)
result<-bm[bm$INS %in% s,]
table(result$Province)


### Slide 21 ########### 
## With the sampling package#
s <- strata(bm,"Province",nh,
            method = "srswor")
result1 <- getdata(bm,s)
head(result1[,c(1:3,ncol(result)-1,
                ncol(result))])

### Slide 22 ##################
### Proportional Allocation ###

n <- 30
gamma <- prop.table(table(bm$Province))
nh <- round(n*gamma)
t(nh)
s <- strata(bm,"Province",nh,"srswor")
result.p <- getdata(bm,s)
nrow(result.p)

### Slide 23 ###################
### Optimal allocation #########
### Variance (true) ############
GetStratVar <- function(Y, sind, nh) {
  Nh <- tapply(sind,sind,length)
  N <- length(sind)
  sum(Nh^2*tapply(Y,sind,function(x) 
    var(x)*(length(x)-1)/length(x))/nh)/N^2
}
GetStratVar(bm$Tot04,bm$Province,
            rep(5,length(unique(bm$Province))))

### Slide 24 #################
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


### Slide 25 ###############

nh <- GetOptAlloc(bm$Tot04,bm$Province,50)
t(nh)
nh2 <- GetOptAlloc(bm$averageincome,bm$Province,50)
t(nh2)

### Slide 26 ###############
### Cluster single stage ###

l <- 4
gamma <- prop.table(table(bm$Province))
clus <- sample(unique(bm$Province),l, 
               prob = gamma, replace = F)
res.clus <- bm[bm$Province %in% clus,]
nrow(res.clus)

### Slide 27 ###############
## Cluster two stage fixed sample size

l <- 4
gamma <- prop.table(table(bm$Province))
clus <- sample(unique(bm$Province),l,
               prob = gamma, replace = F)
fixed.res.clus <-list()
for(i in 1:l){
  nh <- 30
  bm.cl <- bm[bm$Province == clus[i],]
  fixed.res.clus[[i]] <- sample(bm.cl$INS,nh,
                                replace = F)
}
ID <- unlist(fixed.res.clus)
fixed.clus <- bm[bm$INS %in% ID,]
nrow(fixed.clus)


### Slide 28 ##################
l <- 4
sam.clus <- cluster(bm,"Province",4,
                    method = "srswor")
res.clus.samp <- getdata(bm,sam.clus)
nrow(res.clus.samp)

#################################
### Functions Mean and Variance #
#################################

### Slide 29 ##################
### SRS

SRS.mean <- function(Y,S){return(mean(Y[S]))}

SRS.evar <- function(Y,S){return(var(Y[S])/
                                 length(S))}
SRSWOR.evar <- function(Y,S)
{return(fpc(nrow(Y),length(S))*var(Y[S])
        /length(S))}
SRSWOR.evar(bm$averageincome,S)


### Slide 30 ##################
### StrRS ####################
Strat.mean <- function(Y,sind,S){
  Nh <- tapply(Y,sind,length)
  Str.mean <- sum(Nh*tapply(Y[S], sind[S],
                            mean) / sum(Nh))
  return(Str.mean)
}
S <- as.numeric(row.names(result))
Strat.mean(bm$averageincome,bm$Province,S)

### Slide 31 #################
Strat.evar<- function(Y, sind, S) {
  Nh <- tapply(sind,sind,length)
  nh <- tapply(sind[S], sind[S], length)
  ssh <- tapply(Y[S], sind[S], var)
  res <- sum((Nh/sum(Nh))^2*ssh/nh*(Nh-nh)/Nh)
  return(res)
}
S <- as.numeric(row.names(result))
Strat.evar(bm$averageincome,
           bm$Province,S)

### Slide 32 ################
### Mean Cluster ############
SRCS.mean <-function(Y,sind,S){
  L <- length(unique(sind))
  l <- length(unique(sind[S]))
  N <- length(Y)
  N_h_a <- tapply(Y[S],sind[S],length)
  mu_h_a <- tapply(Y[S],sind[S],mean)
  return(L/l*sum(N_h_a/N*mu_h_a))
}
Sc <- as.numeric(row.names(res.clus))
SRCS.mean(bm$averageincome,
          bm$Province,Sc)


### Slide 33 #################
### Variance Cluster #########

se.sq <- function(Y,sind,S){
  L <- length(unique(sind))
  l <- length(unique(sind[S]))
  N <- length(Y)
  mu.SRCS <- SRCS.mean(Y,sind,S)
  c <- N*mu.SRCS/L
  mu_h_a <- tapply(Y[S],sind[S],mean)
  N_h_a <- tapply(Y[S],sind[S],length)
  return ( 1/(l-1)*sum((N_h_a*mu_h_a-c)^2))
}

### Slide 34 #################

SRCS.evar <- function(Y,sind,S){
  L <- length(unique(sind))
  l <- length(unique(sind[S]))
  N <- length(Y)
  part1 <- L^2/N^2
  part2 <- se.sq(Y,sind,S)/l
  part3 <- (L-l)/L
  return(part1*part2*part3)
}
Sc <- as.numeric(row.names(res.clus))
SRCS.evar(bm$averageincome,bm$Province,Sc)


### Slide 36 #################
### Variance equal allocation#
Strat.eq.evar <- function(Y,sind,S){
  L <- length(unique(sind))
  Nh <- tapply(sind,sind,length)
  nh <- tapply(sind[S], sind[S], length)
  ssh <- tapply(Y[S], sind[S], var)
  res <- 1/sum(nh)*sum((Nh/sum(Nh))^2*
                         ssh*((Nh*L-sum(nh))/(Nh-1)))
  return(res)
}

### Slide 37 ##################

Strat.opt.evar <- function(Y, sind, S){
  Nh <- tapply(sind,sind,length)
  nh <- tapply(sind[S], sind[S], length)
  ssh <- tapply(Y[S], sind[S], var)
  part1 <- 1/sum(nh)*sum(Nh/sum(Nh)*sqrt(ssh))^2
  part2 <- 1/sum(Nh)*sum(Nh/sum(Nh)*ssh)
  return(part1-part2)
}












