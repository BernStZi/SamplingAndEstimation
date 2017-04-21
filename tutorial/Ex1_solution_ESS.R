##### Sampling, Weighting and Estimation Exercise 1 ####


### Getting Started ###

install.packages("survey") # Numerous advanced applications for sample survey data
install.packages("foreign") # Load different file-formats (e.g. .sav, .dta (only up to Stata 12))
install.packages("sampling") # Numerous applications for sampling survey data 

library(foreign)
library(survey)
library(sampling)

### Setting work directory ###
main.path <- "F:/ESS_Train2/"
?paste
data.path <- paste(main.path,"Data/",sep="")
setwd(data.path) # sets your working directory - without further instructions, all files that are 
# that are produced will be saved here

### Loading Data Sets ###
?read.table
?read.spss

ESS5.SE <- read.spss("ESS5SE.sav",to.data.frame = TRUE)
ESS5.SE.ctry <- read.spss("ESS5_SE_SDDF.por",to.data.frame = TRUE, use.value.labels = TRUE)
# Don't mind the Warnings #

### merging the data ###

?merge
colnames(ESS5.SE)[5] <- "IDNO"

ESS5.SE.final <- merge(ESS5.SE,ESS5.SE.ctry,by="IDNO",all.x = T)

## Getting rid of the columns that only consist of NAs ##

ESS5.SE.final <- ESS5.SE.final[,colSums(is.na(ESS5.SE.final))<nrow(ESS5.SE.final)] 

### sampling strategy ###
table(ESS5.SE.final$PSU,useNA = "ifany")
# <NA> 
#   1497  -> unclustered sample
table(ESS5.SE.final$STRATIFY,useNA = "ifany")
# 0                              
# 1497 -> no stratification
table(ESS5.SE.final$PROB,useNA = "ifany")
# 0.000382588353 
# 1497 -> SRSWOR due to register based country

summary(ESS5.SE.ctry)

###########################
### The Survey Package ####
###########################


## calculating sample size ##

# Adding a variable to a given data.frame #


ESS5.SE.final$N <- ESS5.SE.final$dweight*ESS5.SE.final$pweight*10000*nrow(ESS5.SE.final)

head(ESS5.SE.final$N)

?svydesign()

ESS5.SE.svy <- svydesign(ids = ~1, data = ESS5.SE.final, fpc = ~N)

svytotal(~tvtot,ESS5.SE.svy )

svymean(~tvtot,ESS5.SE.svy )

## saving data sets ##

save(ESS5.SE,ESS5.SE.ctry,ESS5.SE.final,ESS5.SE.svy,file = "ESS5.Sweden.Rdata")








