options(set.seed(42)) #### setting a global option

###############################
######### Getting Started #####
###############################
path <- "G:/Sand_Summerschool/"
setwd(path)
##### Packages and data sets ##

library(sampling)
data("belgianmunicipalities")
bm<-belgianmunicipalities

##############################
### Prob. of inclusion #######
##############################

### Slide 2 ##################

### pik SRS(WOR) #############
N <- nrow(bm)
n <- 180
pik <- rep(n/N,N)

### pik StrRS ################

Nh <- table(bm$Province)
nh <- c(15,20,35,22,18,22,7,13,20)
pihk <- data.frame(nh/Nh)
names(pihk)[1] <- "Province"
pihk.long <- merge(pihk,bm,by = "Province")

### Sampling package #########

pihk.sa <- inclusionprobastrata(bm$Province,nh)

### Slide 3 ##################

### pik pps ##################
phi <- bm$Tot04/sum(bm$Tot04)
pik <- phi*n
head(pik)

### Slide 4 #################
### pps sampling package ####
pik <- inclusionprobabilities(bm$Tot04,n)
head(pik)
table(pik == 1)



############################
### Design Weighting #######
############################

### Slide 5 #################
### HT-Estimator ############

s <- strata(bm,"Province",nh, "srswor")
samp <- getdata(bm,s)
dk <- 1/samp$Prob
sum(dk*samp$Men04)

sum(bm$Men04)

HTstrata(y = samp$Men04,pik = samp$Prob,strata = samp$Province)

### Slide 6 #################

### Scaling #################
dk.sc <- nrow(samp)*dk/sum(dk)
sum(dk.sc*samp$Men04)/nrow(samp)


### Slide 8 #################
### Truncating ##############

trunc.bounds<-function(di,bound){
  n<-sum(di)
  nopt<-di
  i<-0
  s<-which(di<=0|di<bound[1]|di>bound[2])
  while(i<n){
    if(length(s)!=0){
      s1<-which(nopt<=0)
      s2<-which(nopt<bound[1])
      s3<-which(nopt>bound[2])
      nopt[s1]<-bound[1]
      nopt[s2]<-bound[1]
      nopt[s3]<-bound[2]
      su<-length(s1)*bound[1]+
        length(s2)*bound[1]+length(s3)*bound[2]
      ge<-(n-su)*nopt[-s]/sum(nopt[-s])
      nopt[-s]<-ge
      s<-which(nopt<=0|nopt<bound[1]|
                 nopt>bound[2])}
    if(length(s)!=0){
      i<- i+1
      fi<-i}
    else {
      fi<-i+1
      i<-n
    }
  }
  cat(" number of iterations ",fi,"\n",
      "number of truncated weights ",length(which(nopt%in%bound)),"\n",
      "minimal value",sum(di^2/nopt))
  return(nopt)
}


### Slide 9 ###########

wei <- runif(5000,0.5,7)
table(wei>=6)
bounds <- c(0,6)
wei.trunc <- trunc.bounds(wei,bounds)
table(wei.trunc==6)

### Slide 10 ##########
### Variance HT-Est ###
bm <- bm[-2,]
pik <- inclusionprobabilities(bm$Tot03,30)
IPkl1 <- UPsampfordpi2(pik)
IPkl2 <- UPsystematicpi2(pik)
SIGMA.samp <- IPkl1 - pik%*%t(pik)
SIGMA.syst <- IPkl2 - pik%*%t(pik)

### Slide 11 ###########
var.HT.tot.samp <- t(bm$Tot04/pik
)%*%SIGMA.samp%*%(bm$Tot04/pik)
var.HT.tot.sys<- t(bm$Tot04/pik
)%*%SIGMA.syst%*%(bm$Tot04/pik)
var.HT.tot.samp
var.HT.tot.sys

### Slide 12 ###########
### Variance Estimation: HT est.
s <- UPsystematic(pik)
samp <- getdata(bm,s)
SIGMA.s <- SIGMA.syst[s==1,s==1]
SIGMA.s.tilde <- SIGMA.s/IPkl2[s==1,s==1]
var.hat.HT.tot.syst <- t(samp$Tot04/pik[s==1]
)%*%SIGMA.s.tilde%*%(samp$Tot04/pik[s==1])
var.hat.HT.tot.syst

########################
## Design Effect ######
#######################

### Slide 13 ###########
## design-based approach

var.tot.srs <- var(bm$Tot04)/30*(1-30/nrow(bm))*nrow(bm)^2
deff <- var.HT.tot.samp/var.tot.srs
deff

### Slide 14 ###########
## design-based approach
## In an unclustered sample, the model based approach 
## is only depending on the design effect due to unequal inclusion prob.
deff_p <- sum(30*(1/pik[s==1])^2)/sum(1/pik[s==1])^2
deff_p

## effective sample size
30/deff

30/depp_p

########################
## Poststratification ##
########################

### Slide 15 ###########

## My.pop from day 1
id <- 1:10000
set.seed(42)
education <- sample(c("none","low","average","high"),10000, 
                    replace = T,prob = c(.072,.356,.289,.283))

gender <- sample(c("male","female"),10000,
                 replace = T,prob = c(.488,.512))

iq <- rnorm(10000,100,20)
my.pop <- data.frame(id,gender,education,iq)

##
s <- srswor(100,10000)
samp <- my.pop[s==1,]
genXedu.s <- data.frame(table(samp$gender,samp$education))
genXedu.s[,3] <- genXedu.s[,3]/sum(genXedu.s[,3])
genXedu.pop <- data.frame(table(my.pop$gender,my.pop$education))
genXedu.pop[,3] <- genXedu.pop[,3]/sum(genXedu.pop[,3])
adj.w <- data.frame(genXedu.pop[,3]/genXedu.s[,3])
adj.w[,1]
samp$to.merge <- paste(samp$gender,samp$education)
adj.w$to.merge <- paste(genXedu.s$Var1,genXedu.s$Var2)
adjusted <- merge(adj.w,samp,by="to.merge")

#############################
##### Raking ################
#############################

### Slide 16 ################
age <- rep(as.factor(1:6),times=10)
edu <- rep(as.factor(1:5),each=6,times=2)
gender <- rep(c("m","w"),each=30)
freq <- sample(100,60,replace=T)
# Synthetic sample distribution
samp <- data.frame(age,edu,gender,freq)
samp[,4] <- samp[,4]/sum(samp[,4])
freq2 <- sample(100,60,replace=T)
# in population
master <- data.frame(age,edu,gender,freq2)
master[,4] <- master[,4]/sum(master[,4])
masageXedu <- aggregate(master[,4],
                        list(age=master[,1],edu=master[,2]),sum)
masageXgen <- aggregate(master[,4],
                        list(age=master[,1],gender=master[,3]),sum)
maseduXgen <- aggregate(master[,4],
                        list(edu=master[,2],gender=master[,3]),sum)

### Slide 17 ####################

w_0 <- rep(1,times=nrow(samp))
times <- 0
while(times <= 1000){
  #AgexEducation
  saxe <- aggregate(samp[,4]*w_0,list(age=samp[,1],edu=samp[,2]),sum)
  w_1 <- masageXedu[,3]/saxe[,3]
  w_1 <- rep(w_1,times=(nrow(samp)/nrow(saxe)))
  #AgexGender
  saxg <- aggregate(samp[,4]*w_0*w_1,list(age=samp[,1],gen=samp[,3]),sum)
  w_2 <- masageXgen[,3]/saxg[,3]
  w_2 <- c(rep(w_2[1:(length(w_2)/2)],times=(nrow(samp)/nrow(saxg))),
           rep(w_2[(length(w_2)/2+1):length(w_2)],times=(nrow(samp)/nrow(saxg))))
  #EducationxGender
  sexg <- aggregate(samp[,4]*w_0*w_1*w_2,list(edu=samp[,2],gen=samp[,3]),sum)
  w_3 <- maseduXgen[,3]/sexg[,3]
  w_3 <- rep(w_3,each=(nrow(samp)/nrow(sexg)))
  #w4
  w_4 <- w_0*w_1*w_2*w_3
  if(max(abs(w_0-w_4))>0.05)
  {w_0<-w_4
  times<-times+1}
  else {break}
  cat("iteration",times,"\n")
}
samp$weight<-w_0*samp[,4]


### Slide 18 #####################

sampeduXgen<-aggregate(samp[,5],list(edu=samp[,2],gen=samp[,3]),sum)

sampeduXgen

maseduXgen

##################################
### The survey package ###########
##################################

### Slide 19 ######################

library(Matrix)
library(survey)
bm$pik1 <- inclusionprobabilities(bm$Tot03,100)
s <- UPmaxentropy(bm$pik1)
samp <- getdata(bm,s)
IPkl <- UPmaxentropypi2(bm$pik1)
surv.obj <- svydesign(id=~1,fpc = samp$pik1,
                      data = samp,pps = 
                        ppsmat(IPkl[s==1,s==1]),
                      variance = "YG")

### See Slide 20 - 22 for explanation #

### Slide 22

svytotal(~Tot04,surv.obj,deff=T)


### Slide 23 #################
### Changing from the YG variance to HT variance

surv.obj2 <- svydesign(id=~1,fpc = samp$pik,
                       data = samp,pps = ppsmat(IPkl[s==1,s==1]),variance = "HT")
svytotal(~Tot04,surv.obj2)

##########################################
### Calibration with the survey package ##
##########################################

### Slide 24 ###################

svymean(~averageincome,surv.obj)
mean(bm$averageincome)

## obtaining your population totals #####
lm1 <- lm(averageincome ~Men03+Arrondiss, data=bm)
lm1
pop.tot <- colSums(model.matrix(lm1))
pop.tot

###  Slide 25 ####################

surv.obj3 <- svydesign(id=~0,fpc = samp$pik1,
                       data = samp,pps = "brewer")

### Slide 26 #####################
### Calibration ##################

g_i <- calibrate(surv.obj3,formula =~Men03+Arrondiss
                 ,population=pop.tot,calfun="linear")


### Slide 27 ######################
### Results #######################

svymean(~averageincome,g_i)
svytotal(~Men03,g_i)
svytotal(~Arrondiss,g_i)


mean(bm$averageincome)
sum(bm$Men03)
sum(bm$Arrondiss)


####################################
### Multistage Sampling ############
####################################


#### Slide 28 ######################
data(api)
score <- by(apiclus1,apiclus1$cds,
            function(x)rnorm(x$api.stu,mean = x$api00, sd = sqrt(x$api00)))

l <- 50
nh <- 60
apiclus1$fpc <- inclusionprobabilities(apiclus1$enroll,l)

### Slide 29 ########################

### Sampling Cluster ################

### Selcting a Cluster (PSU) ########
cs <- UPmaxentropy(apiclus1$fpc)
cs.dat <- apiclus1[cs==1,]

### Selction within a school (SSU) ####
score.cs.dat <- score[as.character(cs.dat$cds)]
score.samp <- lapply(score.cs.dat,function(x)x[sample(length(x),nh)])
names(score.samp) <- names(score.cs.dat)
data.s <- data.frame(score = unlist(score.samp),
                     cds=rep(names(score.samp),
                             times=sapply(score.samp,length)))

### Slide 30 ########################
## Merging and second stage incl. prob
DATA.s <- merge(cs.dat,data.s,by="cds")
DATA.s$id <- 1:nrow(DATA.s)
DATA.s$fpc2 <- nh/DATA.s$enroll

## as a survey object ###############

mul.surv <- svydesign(id=~cds+id,fpc = ~fpc+fpc2,
                      data=DATA.s, pps="brewer")
svymean(~api00,mul.surv)
table(weights(mul.surv))

























