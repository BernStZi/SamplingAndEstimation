library(sampling)
library(survey)
data(belgianmunicipalities)


#We want the estimate the total number of Mean in 2004 in Belgium by sampling municipalities
#proportional to their total population in 2003

data(belgianmunicipalities)          #we load the population from the 'survey' package
DATA    <- belgianmunicipalities     
DATA    <- DATA[-2,]                 #we remove a very large municipality (IP>1)
n       <- 25                        #4.3% sample
DATA$IP <- inclusionprobabilities(DATA$Tot03,n)
IPkl    <- UPsampfordpi2(DATA$IP)    #this can take some seconds (we will use Samford sampling)

DELTA   <- IPkl - DATA$IP%*%t(DATA$IP)

#The tue variance 
V <- 
  with(DATA,t(cbind(Men04/IP))%*%DELTA%*%(cbind(Men04/IP))
  )
sqrt(V)

S
set.seed(354)
s       <- UPsampford(DATA$IP) #select a sampling using the Samford's method
DELTA.s <- DELTA[s==1,s==1]
IPkl.s  <-  IPkl[s==1 ,s==1]
DELTA.tilde.s <-DELTA.s/IPkl.s
DATA.s  <-  DATA[s==1,]


## Variances without replacement
## Horvitz-Thompson type
dpps_br <- svydesign(id=~1,  fpc=~IP, data=DATA.s, pps="brewer")
dpps_ov <- svydesign(id=~1,  fpc=~IP, data=DATA.s, pps="overton")
dpps_hr <- svydesign(id=~1,  fpc=~IP, data=DATA.s, pps=HR(sum(DATA$IP^2)/n))
dpps_hr1<- svydesign(id=~1,  fpc=~IP, data=DATA.s, pps=HR())
dpps_ht <- svydesign(id=~1,  fpc=~IP, data=DATA.s, pps=ppsmat(IPkl.s ))
## Yates-Grundy type
dpps_yg  <- svydesign(id=~1,  fpc=~IP, data=DATA.s, pps=ppsmat(IPkl.s),variance="YG")
dpps_hryg<- svydesign(id=~1,  fpc=~IP, data=DATA.s, pps=HR(sum(DATA$IP^2)/n),variance="YG")



V.est <- 
with(DATA.s, t(cbind(Men04/IP))%*%DELTA.tilde.s%*%(cbind(Men04/IP)))



t((DATA.s$Men04/DATA.s$IP)^2)%*%DELTA.tilde.s%*%rep(1,ncol(DELTA.tilde.s))

sqrt(diag(V))
# #sqrt(diag(V.est))


## The with-replacement approximation
dppswr <-svydesign(id=~1, probs=~IP, data=DATA.s)

SE(svytotal(~Men04, dpps_ht)) #should not be used for fixed size designs...
SE(svytotal(~Men04, dpps_yg))

SE(svytotal(~Men04, dpps_hr))

SE(svytotal(~Men04, dpps_hryg))
SE(svytotal(~Men04, dpps_hr1))
SE(svytotal(~Men04, dpps_br))
SE(svytotal(~Men04, dpps_ov))
SE(svytotal(~Men04, dppswr))





data(election)
## high positive correlation between totals
plot(Bush~Kerry,data=election,log="xy")
## high negative correlation between proportions
plot(I(Bush/votes)~I(Kerry/votes), data=election)

## Variances without replacement
## Horvitz-Thompson type
dpps_br<- svydesign(id=~1,  fpc=~p, data=election_pps, pps="brewer")
dpps_ov<- svydesign(id=~1,  fpc=~p, data=election_pps, pps="overton")
dpps_hr<- svydesign(id=~1,  fpc=~p, data=election_pps, pps=HR(sum(election$p^2)/40))
dpps_hr1<- svydesign(id=~1, fpc=~p, data=election_pps, pps=HR())
dpps_ht<- svydesign(id=~1,  fpc=~p, data=election_pps, pps=ppsmat(election_jointprob))
## Yates-Grundy type
dpps_yg  <- svydesign(id=~1,  fpc=~p, data=election_pps, pps=ppsmat(election_jointprob),variance="YG")
dpps_hryg<- svydesign(id=~1,  fpc=~p, data=election_pps, pps=HR(sum(election$p^2)/40),variance="YG")

## The with-replacement approximation
dppswr <-svydesign(id=~1, probs=~p, data=election_pps)

DELTA.tilde.s <-(election_jointprob - election_pps$p%*%t(election_pps$p))/election_jointprob


V.est <- 
  with(election_pps,
       t(cbind(Bush/p,Kerry/p,Nader/p))%*%DELTA.tilde.s%*%(cbind(Bush/p,Kerry/p,Nader/p))
  )


svytotal(~Bush+Kerry+Nader, dpps_ht)
svytotal(~Bush+Kerry+Nader, dpps_yg)
svytotal(~Bush+Kerry+Nader, dpps_hr)
svytotal(~Bush+Kerry+Nader, dpps_hryg)
svytotal(~Bush+Kerry+Nader, dpps_hr1)
svytotal(~Bush+Kerry+Nader, dpps_br)
svytotal(~Bush+Kerry+Nader, dpps_ov)
svytotal(~Bush+Kerry+Nader, dppswr)










