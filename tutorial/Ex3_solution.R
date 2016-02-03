###################
## Exercise 3a ####
###################

#### Getting Started ####


library(foreign)
library(survey)
path <- "F:/ESS Data/"
path.ger <- "F:/ESS Data/Germany/"
setwd(path)

#### Germany ####
Ger.d <- read.spss(paste(path.ger,"ESS5DE.spss/ESS5DE.sav",sep=""),to.data.frame = TRUE)
Ger.ctry <- read.spss(paste(path.ger,"ESS5_DE_SDDF.spss/ESS5_DE_SDDF.por",sep=""),to.data.frame = TRUE, use.value.labels = TRUE)

colnames(Ger.d)[5] <- "IDNO" # In capital letters in the SDDF-file
Ger <- merge(Ger.d,Ger.ctry,by="IDNO", all.x = TRUE)
Ger$PSU <- as.factor(Ger$PSU)
n <- nrow(Ger)
L <- length(unique(Ger$PSU))

Ger <- Ger[,colSums(is.na(Ger))<nrow(Ger)] # To delete all rows that only contain NAs 
Ger$agea <- as.numeric(Ger$agea)

ger.surv <- svydesign(id=~PSU+IDNO,data=Ger,weights = ~dweight)

svymean(~agea,ger.surv,na.rm=TRUE) # survey mean for agea


###################################
### Design Effect (model based) ###
###################################

## Calculating deffp
df.tab <- as.data.frame(table(Ger$PSU,Ger$dweight))
head(df.tab)
deff.tab <- df.tab[df.tab[,3]!=0,]
deff.tab[,2] <- as.numeric(as.character(deff.tab[,2]))

deff_p <- n*sum(deff.tab[,3]*deff.tab[,2]^2)/(sum(deff.tab[,3]*deff.tab[,2])^2)
## Calculating deffc
b <- sum(tapply(Ger$dweight,Ger$PSU,function(x)sum(x)^2))/sum(Ger$dweight^2)
SS <- anova(lm(as.numeric(Ger$agea)~Ger$PSU))
MSB <- SS$`Mean Sq`[1]
MSW <- SS$`Mean Sq`[2]

K <- 1/(L-1)*(n-sum(deff.tab[,3]^2/n))
rho <- (MSB-MSW)/(MSB+(K-1)*MSW)
deff_c <- 1+(b-1)*rho

## Calculating deff (model based)

deff <- deff_p*deff_c

## Calculating neff

neff <- nrow(Ger)/deff

###################
## Exercise 3b ####
###################

url <- "http://raw.githubusercontent.com/BernStZi/SamplingAndEstimation/short/tutorial/Samples_for_EX3b.R"
source(url)

true_income

### Multi-Stage ###
surv <- svydesign(id=~Commune+id,fpc=~prob1+prob2, data=Data.be,pps="brewer")

svymean(~inc,surv)



### Cluster-Sample ###

surv2 <- svydesign(id=~Commune,data = Data.be2,fpc=~N)

svymean(~inc,surv2)


###################
## Exercise 3C ####
###################
#0. load the data
data(api)

#1. build a 'design' object
svy_api <- svydesign(id=~1,strata=~stype, 
                     fpc=~fpc, data=apistrat)

#2. compute pop totals of auxiliaries
lmod <- lm(1:nrow(apipop) ~ stype:api99, data=apipop)
pop.tots <- colSums(model.matrix(lmod)) 
#note that we can use any variable on the left side of "~", because
#we will only use the lm object "lmod" only to extract the population margins in a 
#convenient way

#3. use 'calibrate' to compute the new weights
svy_api.cal <- 
  calibrate(design = svy_api,
            formula = ~ stype:api99,
            population = pop.tots,
            calfun='linear' )

svymean(~api00, svy_api)
svymean(~api00, svy_api.cal)






