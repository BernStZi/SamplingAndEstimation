

library(sampling)
library(survey)
path <- "F:/ESS Data/"
setwd(path)

data(belgianmunicipalities)
bm <- belgianmunicipalities
bm <- bm[-499,] # Delete that entry, because it is too small

bm2 <- bm # for the clustersample

l <- 80
nh <- 300

### income ###
income <- by(bm, bm$INS, function(x)rnorm(x$Tot04, mean=x$averageincome,sd=sqrt(x$averageincome)))

### first stage inclusion probability ####
bm$nu <- bm$Tot03/sum(bm$Tot03)*l
bm$pro <- bm$nu%%1
bm$p <- floor(bm$nu)
bm$prob1 <- bm$nu
bm$prob1[bm$prob1>1] <- 1

set.seed(42)
cs <- UPmaxentropy(bm$pro)
bm$cs <- cs
clus <- bm[cs==1|bm$p>0,]

clus$p <- clus$p+clus$cs



### income to sample ###
nh1 <- nh*clus$p
clus$nh1 <- nh1


clus.inc <- income[as.character(clus$INS)]
inc.samp <- mapply(function(x,y)sample(x,y),clus.inc,nh1)

names(inc.samp) <- names(clus.inc)



### unlist and merge ###


INS <- rep(names(inc.samp),times=sapply(inc.samp,length))

data.inc <- data.frame(inc = unlist(inc.samp), INS = INS)
Data.be <- merge(clus,data.inc,by="INS", all.x=TRUE)


### inclusion probability: second stage ####

Data.be$id <- 1:nrow(Data.be)



## Probability of inclusion second sampling stage
Data.be$prob2 <- Data.be$nh1/Data.be$Tot03
Data.be$prob2[Data.be$nu>1] <- Data.be$prob2[Data.be$nu>1]*Data.be$nu[Data.be$nu>1]/(Data.be$p[Data.be$nu>1])
### 

### Cluster Sample for belgianmunicipalities
# Number of clusters
l2 <- 10 

# Probability of inclusion
bm2$prob1 <-  bm2$Tot03/sum(bm2$Tot03)*l2
set.seed(42)
#Sampling clusters
cs2 <- UPmaxentropy(bm2$prob1)


clus2 <- bm2[cs2==1,]

## Merging
clus.inc2 <- income[as.character(clus2$INS)]


INS <- rep(names(clus.inc2),times=sapply(clus.inc2,length))

data.inc2 <- data.frame(inc = unlist(clus.inc2), INS = INS)
Data.be2 <- merge(clus2,data.inc2,by="INS", all.x=TRUE)

### population size
Data.be2$N <- sum(bm2$Tot03)


