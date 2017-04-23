##### Sampling, Weighting and Estimation Exercise 1 ####


### Getting Started ###
library(foreign)
library(survey)
library(sampling)

### Setting work directory ###
main.path <- "F:/ESS_Train2/"
data.path <- paste(main.path,"Data/",sep="")
setwd(data.path)

### Data ###
## Sweden ## 
load("ESS5.Sweden.Rdata")

## Denmark ##
ESS5.DK <- read.spss("ESS5DK.sav",to.data.frame = TRUE)
ESS5.DK.ctry <- read.spss("ESS5_DK_SDDF.por",to.data.frame = TRUE, use.value.labels = TRUE)
colnames(ESS5.DK)[5] <- "IDNO"
ESS5.DK.final <- merge(ESS5.DK,ESS5.DK.ctry,by="IDNO",all.x = T)
ESS5.DK.final <- ESS5.DK.final[,colSums(is.na(ESS5.DK.final))<nrow(ESS5.DK.final)] 
ESS5.DK.final$N <- ESS5.DK.final$dweight*ESS5.DK.final$pweight*10000*nrow(ESS5.DK.final)

## Reduce dataset to relevant variables ##

DK_tv <- data.frame(tvtot = as.character(ESS5.DK.final$tvtot), N = ESS5.DK.final$N, 
                    cntry = as.character(ESS5.DK.final$cntry))

SE_tv <- data.frame(tvtot = as.character(ESS5.SE.final$tvtot), N = ESS5.SE.final$N, 
                    cntry = as.character(ESS5.SE.final$cntry))

## More than 3 hours of tv consumption ### 

DK_tv$mt3 <- 0
DK_tv$mt3[DK_tv$tvtot == "More than 3 hours"] <- 1
SE_tv$mt3 <- 0
SE_tv$mt3[SE_tv$tvtot == "More than 3 hours"] <- 1

## Combining the countries ###

SE_DK <- rbind(SE_tv,DK_tv)

### survey objects ### 

#####################################
## seperate empirical distribution ##
#####################################

DK_svy <- svydesign(id=~1,fpc = ~N,data = DK_tv)
SE_svy <- svydesign(id=~1,fpc = ~N,data = SE_tv)
# -> both data sets have srs design
stab_DK <- svytable(~tvtot,DK_svy)
stab_DK
svymean(~mt3,DK_svy)
svytotal(~mt3,DK_svy)

stab_SE <- svytable(~tvtot,SE_svy)
stab_SE 
svymean(~mt3,SE_svy)
svytotal(~mt3,SE_svy)




#######################################
### combined empirical distribution ###
#######################################

SE_DK_svy <- svydesign(id=~1,fpc = ~N,strata = ~cntry ,data = SE_DK)
# -> stratified design
stab_SE_DK <- svytable(~tvtot,SE_DK_svy)
stab_SE_DK
svymean(~mt3,SE_DK_svy)
svytotal(~mt3,SE_DK_svy)

### Plotting the results ### 
library(lattice)
barchart(stab_DK)
barchart(stab_SE)
barchart(stab_SE_DK)


#####################################
########### Regressions #############
#####################################

### data frame of relevant variables ###
?as.integer


DK_short <- data.frame( N = ESS5.DK.final$N, pspwght = ESS5.DK.final$pspwght, 
                        pwght = ESS5.DK.final$pweight, cntry = as.character(ESS5.DK.final$cntry),
                        age = ESS5.DK.final$agea,hhsize=ESS5.DK.final$hhmmb,
                        edu =  as.integer(ESS5.DK.final$eisced),wkht = ESS5.DK.final$wkhtot,
                        hhinc = ESS5.DK.final$hinctnta)

DK_short$postweight <- DK_short$pspwght*DK_short$pwght*10000

DK_short$Inc <- as.integer(DK_short$hhinc)

SE_short <- data.frame( N = ESS5.SE.final$N, pspwght = ESS5.SE.final$pspwght, 
                        pwght = ESS5.SE.final$pweight, cntry = as.character(ESS5.SE.final$cntry),
                        age = ESS5.SE.final$agea,hhsize=ESS5.SE.final$hhmmb,
                        edu =  as.integer(ESS5.SE.final$eisced),wkht = ESS5.SE.final$wkhtot,
                        hhinc = ESS5.SE.final$hinctnta)

SE_short$postweight <- SE_short$pspwght*SE_short$pwght*10000


SE_short$Inc <- as.integer(SE_short$hhinc)

Joined <- rbind(DK_short,SE_short)

##### the linear model ####
?lm

# the argument "weights" is needed to conduct a weighted least squares regression

?summary # -> summary of relevant results

summary(lm(Inc~age+hhsize+wkht+edu, data=Joint,weights = postweight))


### The generalised linear model ##


Joined.svy <- svydesign(id=~1, data = Joint, fpc = ~N, strata = ~cntry, weights = ~postweight)
# weights as an additional argument

summary(svyglm(Inc~age+hhsize+wkht+edu, Joint.svy, family = "gaussian"))
# family "gaussion" refers to a linear model

# Point estimates for coefficients are equal, but their variance differs













