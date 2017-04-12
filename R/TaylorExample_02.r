
#library(MASS)
library(Matrix)  #need this for the ppsmat
library(mvtnorm)
library(survey)
library(sampling) 
library(PracTools) 




#Example with
library(PracTools) #load the package
data(smho.N874)    #load the data set
smho. <- smho.N874[smho.N874$hosp.type != 4, ]
smho.$hosp.type <- as.factor(smho.$hosp.type)
smho.$FINDIRCT  <- as.factor(smho.$FINDIRCT)
lmod1 <- lm(EXPTOTAL ~ SEENCNT + EOYCNT + FINDIRCT + hosp.type:BEDS, data=smho.)
tab.model <- xtable(summary(lmod1),digits = 2,caption = "Model Summary")
print(tab.model,caption.placement = "top")


smho.[,"BEDS"] <-   # before sampling order the data set by hospital type
  smho.[order(smho.$hosp.type),"BEDS"] 

x <- smho.[,"BEDS"]
x[x <= 5] <- 5      #recode small hospitals to have a minimum size
x <- sqrt(x)

n <- 80             #sample size
IP  <- n*x/sum(x)

set.seed(428274453)
sam <- UPsampford(IP)

sam.dat    <- smho.[sam==1, ]
sam.dat$IP <- IP[sam==1]   

#1. build a 'design' object
smho.dsgn <- 
  svydesign(ids = ~1,       # no clusters
            strata = NULL,  # no strata
            data = sam.dat, # the sample data 
            fpc = ~IP,      # inclusion probabilities
            pps = "brewer") # handeling of 2. order inc.prob.

lmod2 <- lm(EXPTOTAL ~ SEENCNT + EOYCNT + hosp.type:BEDS, data=smho.)
pop.tots <- colSums(model.matrix(lmod2)) 

sam.lin1 <- 
  calibrate(design = smho.dsgn,
            formula = ~ SEENCNT + EOYCNT + hosp.type:BEDS,
            population = pop.tots,
            calfun='linear' )
#Estimates the linerized variance with Brewer approximation 
svymean(~EXPTOTAL,design=sam.lin1)
svymean(~EXPTOTAL,design=smho.dsgn)


#Taylor linearization of an estimator for the regression coefficient.
UPsystematic_support <- 
function (pik) 
{
  n = sum(pik)
  n = .as_int(n)
  pik1 = pik[pik > 0 & pik < 1]
  N = length(pik1)
  Vk = cumsum(pik1)
  Vk1 = Vk%%1
  if (Vk1[N] != 0) 
    Vk1[N] = 0
  r = c(sort(Vk1), 1)
  cent = (r[1:N] + r[2:(N + 1)])/2
  p = r[2:(N + 1)] - r[1:N]
  A = matrix(c(0, Vk), nrow = N + 1, ncol = N) - t(matrix(cent, 
                                                          nrow = N, ncol = N + 1))
  A = A%%1
  M = matrix(as.integer(A[1:N, ] > A[2:(N + 1), ]), N, N)
  list(support=M,p=p)
}

strat.sample <- function(sind,n.h){
  N.h   <- table(sind)[names(n.h)] 
  N     <- length(sind)
  sam   <- mapply(function(x,y)sample(x,y),N.h,n.h)
  sam   <- mapply(function(x,y)x[y],split(1:N,sind)[names(n.h)] ,sam)
  as.numeric(1:N%in%unlist(sam))
}
strat.sample.pi2 <- function(sind,n.h){
  N.h   <- table(sind)[names(n.h)] 
  N     <- length(sind)
  id.strat <- 
   split(1:N,sind)[names(n.h)]
  
  IPkl <- array(NA,rep(N,2))
  for(i in names(id.strat)){
    for(j in names(id.strat)){
    if(i==j){    
     IPkl[id.strat[[i]],id.strat[[j]]] <- (n.h[[i]]*(n.h[[i]]-1))/(N.h[[i]]*(N.h[[i]]-1))
    }else{
     IPkl[id.strat[[i]],id.strat[[j]]] <- n.h[[i]]/N.h[[i]]*n.h[[j]]/N.h[[j]]
    }
   }    
  }
  IPk <- as.vector(n.h[sind]/N.h[sind])
  diag(IPkl)  <- IPk  
  IPkl
}

data(smho.N874)    
sigma <- cov(smho.N874)
mu <- colMeans(smho.N874)+c(0,100,100,100,10,10)

DATA <- rmvnorm(n=nrow(smho.N874), sigma = sigma/10, mean = mu)
DATA <- as.data.frame(DATA)
DATA$hosp.type <- as.factor(smho.N874$hosp.type)

#True Beta
#smho <- smho.N874[smho.N874$BEDS>0, ]
#smho$hosp.type <- as.factor(smho$hosp.type)
DATA  <- DATA[order(DATA$hosp.type),]
mod.formula <- EXPTOTAL ~ SEENCNT + EOYCNT +  BEDS
lmod1 <- lm(mod.formula, data=DATA)

n   <- 80
# V.h <- tapply(DATA$EXPTOTAL,DATA$hosp.type,sd)
# N.h <- table(DATA$hosp.type)
# n.h <- N.h*V.h/(sum(N.h*V.h))*n
# n.h <- round(n.h)

X <- t(model.matrix(lmod1))
y <- DATA[,1]

BETA <- solve(X%*%t(X))%*%X%*%y




         #sample size
#IPk  <- inclusionprobabilities(DATA$BEDS,n)
#IPkl <- UPsampfordpi2(IPk)
IPkl <- strat.sample.pi2(DATA$hosp.type,n.h)
IPk  <- diag(IPkl)


#su   <- UPsystematic_support(IPk)
#IPkl <- UPsystematicpi2(IPk)

DATA$d    <- 1/IPk
DATA$prob <- IPk
DATA$fpc  <- as.vector(N.h[DATA$hosp.type])
SIGMA     <- IPkl - IPk%*%t(IPk)


#sample

set.seed(42)
sam<-strat.sample(DATA$hosp.type,n.h)
#sam  <- UPsampford(IPk)
#sam  <- UPsystematic(IPk)

X.s <- X[,sam==1]
y.s <- y[sam==1]
IPk.s <- IPk[sam==1]

DATA.s <- DATA[sam==1,]

W <- diag(1/IPk.s)

BETA.hat <- solve(X.s%*%W%*%t(X.s))%*%X.s%*%W%*%y.s
wlmod1   <- lm(mod.formula, data=DATA.s, weights =DATA.s$d )
sum.wlmod1<-summary(wlmod1)
#check the point estimates are the same...

#Approximate variance
res <- y- t(X)%*%BETA
w.res  <- res/IPk

res.s <- y.s- t(X.s)%*%BETA.hat
w.res.s  <- res.s/IPk[sam==1]

SIGMA.hat <- SIGMA[sam==1,sam==1]/IPkl[sam==1,sam==1]


V <- V.s<- array(NA,rep(nrow(X),2))
for(i in 1:nrow(X)){
  for(j in 1:nrow(X)){
    
  V[i,j] <- t(X[i,]*w.res)%*%SIGMA%*%(X[j,]*w.res)
  V.s[i,j] <-   t(X.s[i,]*w.res.s)%*%SIGMA.hat%*%(X.s[j,]*w.res.s)
 }
}


# all.beta <-
# lapply(1:ncol(su$support),function(x){
#   s.dat    <- DATA[su$support[,x]==1,]
#   coef(lm(EXPTOTAL ~ SEENCNT + EOYCNT +  BEDS, data=s.dat, weights = s.dat$d ))
# })
#   
# 
# V.BETA <- 
# apply(mapply(function(x,y)x^2*y,all.beta,su$p),1,sum) - 
#   apply(mapply(function(x,y)(x*y),all.beta,su$p),1,sum)^2


AV.BETA <- solve(X%*%t(X))%*%V%*%solve(X%*%t(X))

AV.est.BETA <- solve(X.s%*%W%*%t(X.s))%*%V.s%*%solve(X.s%*%W%*%t(X.s))

#sigma.sq      <- sum((residuals(lmod1)^2))/nrow(lmod1$model)
w.S.sq         <- sum((residuals(wlmod1)^2)*wlmod1$weights)/sum.wlmod1$df[2]
V.est.BETA.mod <- w.S.sq*solve(t(model.matrix(wlmod1))%*%(diag(wlmod1$weights))%*%(model.matrix(wlmod1)))

#V.hat.BETA.mod <- sigma.sq*solve(t(model.matrix(wlmod1))%*%(diag(wlmod1$weights))%*%(model.matrix(wlmod1)))

sqrt(diag(V.est.BETA.mod))
SE(lm(EXPTOTAL ~ SEENCNT + EOYCNT +  BEDS, data=DATA.s, weights = DATA.s$d))


#doing the same with the survey package
nhis.dsgn <-
  svydesign( ids  = ~0
            ,strata =  ~hosp.type
            ,data = DATA.s
            ,fpc = ~fpc
            #,pps=ppsmat(IPkl[sam==1,sam==1])
            #,variance =  "YG"
            )
#nhis.dsgn.approx <- 
#  svydesign(ids = ~0,
#            data = DATA.s,
#            fpc = ~prob,
            #pps="brewer")


lm.svy <- svyglm(lmod1$terms, design = nhis.dsgn)
sqrt(diag(AV.est.BETA.hat))
SE(lm.svy)


#Simulation
#SIG <- ppsmat(IPkl[sam==1,sam==1])
R  <- 10000
pb <- txtProgressBar(min = 0, max = R, style = 3)
sim.OUT <- 
 vapply( 1:R
        ,function(x){
           #s.       <- UPsystematic(IPk)==1
           s. <- strat.sample(DATA$hosp.type,n.h)==1
           s.dat    <- DATA[s.,]
           #svyd     <- 
           #svydesign( ids = ~0, data = s.dat, fpc= ~prob
          #            ,pps="brewer"#ppsmat(IPkl[s.==1,s.==1])
#                      ,variance =  "YG" 
           #           )
           
           svyd     <- 
           svydesign(ids  = ~1
                     ,strata =  ~hosp.type
                     ,data = s.dat
                     ,fpc = ~fpc
                     )
           wlm      <- lm(EXPTOTAL ~ SEENCNT + EOYCNT +  BEDS, data=s.dat, weights = s.dat$d )
           svy.WGLS <- svyglm(EXPTOTAL ~ SEENCNT + EOYCNT +  BEDS, design = svyd)
           setTxtProgressBar(pb, x)
           cbind(est=coef(wlm), sd.model=SE(wlm), sd.desgin=SE(svy.WGLS))
        }
        ,FUN.VALUE = array(0,c(4,3)) )

close(pb)

apply(sim.OUT,c(1,2),mean)[,2:3]/sqrt(diag(AV.BETA))-1
apply(sim.OUT,c(1,2),mean)[,2:3]/apply(sim.OUT,c(1,2),sd)[,1]-1

#Hart to say what the better benchmark is MC.V or AV
str(sim.OUT)

#           svydesign( ids = ~0, data = s.dat, prob = ~prob
#                                 ,pps=ppsmat(IPkl[s.==1,s.==1])
#                                 ,variance =  "YG" )


