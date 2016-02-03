#### Sampling, Weighting and Estimation 04.02.2016 ####
#### Day 1 #####
#### Getting Started ####


install.packages("survey") # Numerous advanced applications for sample survey data
install.packages("foreign") # Load different file-formats (e.g. .sav, .dta (only up to Stata 12))

library(foreign)
library(survey)


### Setting work directory ####

main.path <- "H:/R_Course_SWE_GRADE/Exercise1/"
data.path <- paste(main.path,"data/",sep="")
setwd(data.path)
### Loading data ###
ESS.Sweden <- read.spss("ESS5SE.sav",to.data.frame = TRUE)
ESS.Sweden.Ctry <- read.spss("ESS5_SE_SDDF.por",to.data.frame = TRUE, use.value.labels = TRUE)
# Don't mind the Warnings #

### Merging the Data ####

?merge

colnames(ESS.Sweden)[5] <- "IDNO"
ESS <- merge(ESS.Sweden,ESS.Sweden.Ctry,by="IDNO",all.x = TRUE)

ESS <- ESS[,colSums(is.na(ESS))<nrow(ESS)] # delete all columns that consist only of NAs



table(ESS$PSU,useNA = "ifany")
# <NA> 
#   1497  -> unclustered sample

table(ESS$STRATIFY,useNA = "ifany")
# 0                              
# 1497 -> no stratification


table(ESS$PROB)

# 0.000382588353 
# 1497            -> SRSWOR due to register based country

summary(ESS.Sweden.Ctry)


### The Survey Package ###
ESS$N <- ESS$dweight*ESS$pweight*10000*nrow(ESS)

?svydesign()

ESS.svy <- svydesign(ids = ~1, data = ESS, fpc = ~N)

svytotal(~tvtot,ESS.svy)

svymean(~tvtot,ESS.svy)