library(survey)
library(sampling)

strSR.sample <- function(strind, nh, replace=FALSE){
  Nh   <- table(strind)[names(nh)]
  h.id <- split(1:sum(Nh), strind)[names(nh)]
  sam <- mapply(function(x,y) sample(x, y, replace=replace), Nh, nh, SIMPLIFY = F)
  sam <- unlist(mapply(function(x,y) x[y]
                       , h.id
                       , sam, SIMPLIFY = F))
  sam
}


data(api)

POP <- apipop
POP$qapi99    <- cut(POP$api99, quantile(POP$api99), include.lowest = T)

POP$Nh        <- table(POP$stype)[POP$stype]

Nh.tab        <- table(POP$stype)
Nh.vec        <- as.vector(Nh)
tab.names     <- apply(expand.grid(dimnames(Nh.tab)),1,paste,collapse="_")
names(Nh.vec) <- tab.names

mosaicplot(Nh.tab)



#sample size
n <- 60 #ceiling(dim(POP)[1]*0.01)

##allocations

#equal
nh.eq <-  rep(n/length(Nh.tab),length(Nh.tab))
names(nh.eq) <- tab.names
#proportional
nh.pr <- round(Nh.tab/sum(Nh.vec)*n)



#optimal
V.h   <- tapply(POP$api99,POP$stype,sd)[tab.names]
nh.op <- round((Nh.tab*V.h)/(sum(Nh.tab*V.h))*n)



##select sample

R <- 2000
set.seed(1133)
OUT <- 
sapply(1:R,function(x){
s.eq <- strSR.sample(POP$stype, nh.eq, replace=FALSE)
s.pr <- strSR.sample(POP$stype, nh.pr, replace=FALSE)
s.op <- strSR.sample(POP$stype, nh.op, replace=FALSE)


strSRS.eq <- POP[s.eq,]
strSRS.pr <- POP[s.pr,]
strSRS.op <- POP[s.op,]


#options(survey.lonely.psu="adjust")

svystrSRS.eq <- svydesign(ids=~cds, strata =~stype,  fpc=~Nh, data=strSRS.eq)
svystrSRS.pr <- svydesign(ids=~cds, strata =~stype,  fpc=~Nh, data=strSRS.pr)
svystrSRS.op <- svydesign(ids=~cds, strata =~stype,  fpc=~Nh, data=strSRS.op)

c( eq=SE(svymean(~api00, svystrSRS.eq))
  ,pr=SE(svymean(~api00, svystrSRS.pr))
  ,op=SE(svymean(~api00, svystrSRS.op))
)
})


