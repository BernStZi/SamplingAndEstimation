#Taylor linearization of an estimator for the regression coefficient.

#library(MASS)
library(Matrix)  #need this for the ppsmat
library(mvtnorm)
library(survey)
library(sampling) 
library(PracTools) 
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
V.h <- tapply(DATA$EXPTOTAL,DATA$hosp.type,sd)
N.h <- table(DATA$hosp.type)
n.h <- N.h*V.h/(sum(N.h*V.h))*n
n.h <- round(n.h)

X <- t(model.matrix(lmod1))
y <- DATA[,1]

BETA <- solve(X%*%t(X))%*%X%*%y

id.strat <- 
 split(1:nrow(DATA),DATA$hosp.type)

IPkl <- array(NA,rep(nrow(DATA),2))
for(i in names(id.strat)){
  for(j in names(id.strat)){
  if(i==j){    
   IPkl[id.strat[[i]],id.strat[[j]]] <- (n.h[[i]]*(n.h[[i]]-1))/(N.h[[i]]*(N.h[[i]]-1))
  }else{
   IPkl[id.strat[[i]],id.strat[[j]]] <- n.h[[i]]/N.h[[i]]*n.h[[j]]/N.h[[j]]
     
  }
 }    
}

         #sample size
IPk  <- inclusionprobabilities(DATA$BEDS,n)
IPkl <- UPsampfordpi2(IPk)
#IPk  <- inclusionprobabilities(DATA$BEDS,n)
#IPkl <- UPsystematicpi2(IPk)

DATA$d <- 1/IPk
DATA$prob <-IPk

SIGMA <- IPkl - IPk%*%t(IPk)


set.seed(42)
sam   <- mapply(function(x,y)sample(x,y),N.h,n.h)
sam   <- mapply(function(x,y)x[y],split(1:nrow(DATA),DATA$hosp.type),sam)


#sam  <- UPsystematic(IPk)

X.s <- X[,sam==1]
y.s <- y[sam==1]
IPk.s <- IPk[sam==1]

DATA.s <- DATA[sam==1,]

W <- diag(1/IPk.s)

BETA.hat <- solve(X.s%*%W%*%t(X.s))%*%X.s%*%W%*%y.s
wlmod1   <- lm(mod.formula, data=DATA.s,weights =DATA.s$d )
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

AV.BETA <- solve(X%*%t(X))%*%V%*%solve(X%*%t(X))
sqrt(diag(AV.BETA))

AV.BETA.hat <- solve(X.s%*%W%*%t(X.s))%*%V.s%*%solve(X.s%*%W%*%t(X.s))
sqrt(diag(AV.BETA.hat))

sigma.sq      <- sum((residuals(lmod1)^2))/nrow(lmod1$model)

w.S.sq        <- sum((residuals(wlmod1)^2)*wlmod1$weights)/sum.wlmod1$df[2]

V.est.BETA.mod <- w.S.sq*solve(t(model.matrix(wlmod1))%*%(diag(wlmod1$weights))%*%(model.matrix(wlmod1)))

#V.hat.BETA.mod <- sigma.sq*solve(t(model.matrix(wlmod1))%*%(diag(wlmod1$weights))%*%(model.matrix(wlmod1)))

sqrt(diag(V.est.BETA.mod))



#doing the same with the survey package
nhis.dsgn <- 
  svydesign(ids  = ~0,
            data = DATA.s,
            fpc = ~prob,
            pps=ppsmat(IPkl[sam==1,sam==1]), variance =  "YG")
nhis.dsgn.approx <- 
  svydesign(ids = ~0,
            data = DATA.s,
            fpc = ~prob,
            pps = "brewer")


lm.svy <- svyglm(lmod1$terms, design = nhis.dsgn)
summary(lm.svy)

lm.svy.approx <- svyglm(lmod1$terms, design = nhis.dsgn.approx)
summary(lm.svy.approx)

#Simulation
#SIG <- ppsmat(IPkl[sam==1,sam==1])
R  <- 10000
pb <- txtProgressBar(min = 0, max = R, style = 3)
sim.OUT <- 
 vapply( 1:R
        ,function(x){
           s.       <- UPsystematic(IPk)==1
           s.dat    <- DATA[s.,]
           svyd     <- svydesign( ids = ~0, data = s.dat, prob = ~prob
                                 ,pps=ppsmat(IPkl[s.==1,s.==1])
                                 ,variance =  "YG" )
           wlm      <- lm(EXPTOTAL ~ SEENCNT + EOYCNT +  BEDS, data=s.dat, weights = 1/s.dat$d )
           svy.WGLS <- svyglm(mod.formula, design = svyd)
           setTxtProgressBar(pb, x)
           cbind(est=coef(wlm), sd.model=SE(wlm), sd.desgin=SE(svy.WGLS))
        }
        ,FUN.VALUE = array(0,c(4,3)) )

close(pb)


sqrt(diag(AV.BETA))/apply(sim.OUT,1,sd)-1
sqrt(diag(AV.BETA.hat))/apply(sim.OUT,1,sd)-1
sqrt(diag(V.est.BETA.mod))/apply(sim.OUT,1,sd)-1

str(sim.OUT)




