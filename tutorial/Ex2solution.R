## ----message=F,warning=F-------------------------------------------------
library(foreign)

## ----echo=F--------------------------------------------------------------
Ex <- T
library(knitr)
set.seed(1133)

## ----echo=F,eval=F-------------------------------------------------------
## main.path <- "C:/Users/kolbjp/Documents/GitHub/SamplingAndEsimation/tutorial"
## 
## main.path <- "C:/Users/Kolb/Documents/GitHub/SamplingAndEstimation/tutorial"
## 
## setwd(main.path)
## purl("Ex2solution.Rmd")

## ----echo=F,eval=Ex------------------------------------------------------
## Set working directory
main.path <- "J:/Work/Statistik/Kolb/Workshops/2016/grade_sampling/"

data.path <- paste(main.path,"data/",sep="")

setwd(data.path)

## ----eval=Ex-------------------------------------------------------------
DK <- read.spss("ESS5DK.sav",to.data.frame=T)
SE <- read.spss("ESS5SE.sav",to.data.frame=T)

## ----eval=Ex-------------------------------------------------------------
DK$N <- DK$pweight*10000*nrow(DK)

SE$N <- SE$pweight*10000*nrow(SE)

## ----eval=Ex-------------------------------------------------------------
DK_tv <- data.frame(tvtot=as.character(DK$tvtot),
                    N=DK$N,
                    cntry=as.character(DK$cntry))

## ----eval=Ex-------------------------------------------------------------
SE_tv <- data.frame(tvtot=as.character(SE$tvtot),
                    N=SE$N,
                    cntry=as.character(SE$cntry))

## ----eval=Ex-------------------------------------------------------------
NE <- rbind(DK_tv,SE_tv)

## ----eval=Ex-------------------------------------------------------------
NE$mt3 <- 0
NE$mt3[NE$tvtot=="More than 3 hours"] <- 1  

## ----message=F-----------------------------------------------------------
library(survey)

## ----eval=Ex-------------------------------------------------------------
# SRS design
svydes_DK <- svydesign(id=~1,fpc=~N, data=DK)
svydes_SE <- svydesign(id=~1,fpc=~N, data=SE)

## ----eval=Ex-------------------------------------------------------------
# Stratified design
svydes_NE <- svydesign(id=~1,strata=~cntry, 
                       fpc=~N, data=NE)

## ----eval=Ex-------------------------------------------------------------
stab_DK <- svytable(~tvtot,svydes_DK)
stab_DK

## ----eval=Ex-------------------------------------------------------------
stab_SE <- svytable(~tvtot,svydes_SE)
stab_SE

## ----eval=Ex-------------------------------------------------------------
stab_NE <- svytable(~tvtot,svydes_NE)
stab_NE

## ----eval=Ex-------------------------------------------------------------
# R-package for visualisation

library(lattice)
barchart(stab_DK)
barchart(stab_SE)

## ----eval=Ex-------------------------------------------------------------
svytotal(~mt3,svydes_NE)

svymean(~mt3,svydes_NE)

## ----message=F-----------------------------------------------------------
library(survey)
data(api)

## ----echo=F--------------------------------------------------------------
kable(apipop[1:8,1:5])

## ------------------------------------------------------------------------
svy_api <- svydesign(id=~1,strata=~stype, 
                       fpc=~fpc, data=apistrat)

svymean(~api00,svy_api)

## ------------------------------------------------------------------------
strSRsample <- function(strind, nh, replace=FALSE){
  Nh <- table(strind)[names(nh)]
  h.id <- split(1:sum(Nh), strind)[names(nh)]
  
  
  sam <- mapply( function(x,y) sample(x, y,
                  replace=replace)
                 , Nh, nh, SIMPLIFY = F)
  unlist(mapply(function(x,y) x[y]
                , h.id
                , sam, SIMPLIFY = F)
         ,use.names = FALSE)
}

## ----eval=F--------------------------------------------------------------
## library(devtools)
## install_github("BernStZi/SamplingAndEstimation/r/
##                sampaest",
##                ref="short")

## ----eval=F--------------------------------------------------------------
## url <- "http://raw.githubusercontent.com/BernStZi/
## SamplingAndEstimation/short/r/sampaest/R/strSRsample.R"
## source(url)

## ------------------------------------------------------------------------
nh.eq <- c(20,20,20)
names(nh.eq) <- names(table(apipop$stype))

s.eq <- strSRsample(apipop$stype, nh.eq, replace=FALSE)

## ------------------------------------------------------------------------
Nh.tab  <- table(apipop$stype)
n <- 60
nh.pr <- round(Nh.tab/sum(Nh.tab)*n)

s.pr <- strSRsample(apipop$stype, nh.pr, replace=FALSE)

## ----echo=F,eval=T-------------------------------------------------------
V.h   <- tapply(apipop$api99,apipop$stype,sd)[names(Nh.tab)]
nh.op <- round((Nh.tab*V.h)/(sum(Nh.tab*V.h))*n)

## ------------------------------------------------------------------------
s.op <- strSRsample(apipop$stype, nh.op, replace=FALSE)

## ------------------------------------------------------------------------
pop <- apipop
pop$Nh <- table(apipop$stype)[apipop$stype]
strSRS.eq <- pop[s.eq,]
strSRS.pr <- pop[s.pr,]
strSRS.op <- pop[s.op,]

## ------------------------------------------------------------------------
svystrSRS.eq <- svydesign(ids=~cds, strata =~stype,  
                          fpc=~Nh, data=strSRS.eq)

svymean(~api00, svystrSRS.eq)

## ------------------------------------------------------------------------
svystrSRS.pr <- svydesign(ids=~cds, strata =~stype,  
                          fpc=~Nh, data=strSRS.pr)

svymean(~api00, svystrSRS.pr)

## ------------------------------------------------------------------------
svystrSRS.op <- svydesign(ids=~cds, strata =~stype,  
                          fpc=~Nh, data=strSRS.op)

svymean(~api00, svystrSRS.op)

