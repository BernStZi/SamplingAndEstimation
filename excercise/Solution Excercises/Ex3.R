#################################
## Setting the Directory ########
#################################

path <- "H:/Sand_Summerschool/Excersises/"
setwd(path)
library(sampling)
data("belgianmunicipalities")
bm <- belgianmunicipalities

#########################
## Excersise 4 ##########
#########################

L <- 20
aggr <- aggregate(bm$Tot03,list(Arrondiss=bm$Arrondiss),sum)
###sampling Prortional to Tot03
aggr$pik <- inclusionprobabilities(aggr$x,L)
set.seed(42)
s <- UPmaxentropy(pik)
dat <- getdata(aggr,s)
DATA.c <- merge(bm,dat,by="Arrondiss")
### Determining the weights per cluster and cluster sizes
wei <- data.frame(Arrondiss=unique(DATA.c$Arrondiss),pik = aggr$pik[s==1],
                  nh = tapply(DATA.c$Arrondiss,DATA.c$Arrondiss,length))
### Calculating deffp
deffp <- nrow(DATA.c)*sum((1/DATA.c$pik)^2)/(sum(1/DATA.c$pik)^2)
### Calcluating b
b <- mean(wei$nh)
### MSW and MSB 
SS <- anova(lm(DATA.c$Tot04~DATA.c$Arrondiss))
MSB <- SS$`Mean Sq`[1]
MSW <- SS$`Mean Sq`[2]

K <- 1/(L-1)*(nrow(DATA.c)-sum(wei$nh^2/nrow(DATA.c)))
### rho
rho <- (MSB-MSW)/(MSB+(K-1)*MSW)
### deffc
deffc <- 1+(b-1)*rho
### deff modelbased
deff <- deffp*deffc
deff

##### designbased deff
#Variance SRSWOR
nn <-mean(tapply(bm$Arrondiss,bm$Arrondiss,length))*L
srs.var<-var(bm$Tot04)/nn*(1-nn/nrow(bm))


### Getting the Variance of DATA.c
sv <- svydesign(id=~Arrondiss, fpc=~pik, data=DATA.c, pps="brewer")

### Designbased
SE(svymean(~Tot04,sv))^2/srs.var

# svymean(~Tot04,sv,deff=T)
# mean    SE DEff
# Tot04 18142  2088 4.36 # Deff in survey package does not work properly!!!!!!!!!!!!

### neff
nrow(DATA.c)/deff



#########################
## Excersise 5 ##########
#########################

### cluster and sample sizes
l <- 50
nh <- 80

### Generating the income variable from averageincome
income <-  by(bm,bm$INS,function(x)rnorm(x$Tot04,mean=x$aver,sd=sqrt(x$aver)))

### inclusion probabilites and sampling

bm$fpc1 <- inclusionprobabilities(bm$Tot03,l)
set.seed(42)
clus <- UPmaxentropy(bm$fpc1)
clus.dat <- bm[clus==1,]

### the income variable
inc.clus.sam <-  income[as.character(clus.dat$INS)]

### sampling within each cluster
set.seed(42)
inc.samp <-   lapply(inc.clus.sam,function(x)x[sample(length(x),nh)])
names(inc.samp) <- names(inc.clus.sam)

### unlist the data of the sample and merging
data.s <- data.frame(inc=unlist(inc.samp) ,
                     INS=as.numeric(rep(names(inc.samp),times=sapply(inc.samp,length))))  

DATA.s <-   merge(clus.dat,data.s ,by="INS")

### fpc2 and ids for the individuals
DATA.s$id <- 1:nrow(DATA.s)
DATA.s$fpc2 <- nh/DATA.s$Tot03

### defining the survey object

ab <- svydesign(id=~Commune+id, fpc=~fpc1+fpc2, data=DATA.s, pps="brewer")

### survey mean and total

svymean(~inc, design=ab,deff=T)

nn1 <- 80*l
inc <- unlist(income)
srs2.var <- var(inc)/nn1*(1-nn1/length(inc))
SE(svymean(~inc,ab))^2/srs2.var
# 70.23436

table(weights(ab))
svytotal(~inc,ab)

### calibration

lm1<-lm(averageincome~Men03,data=bm)
pop.tot <- colSums(model.matrix(lm1))
wi <- calibrate(ab, formula=~Men03,population=pop.tot,calfun = "linear")
svytotal(~Men03, wi)
svymean(~inc,wi)

