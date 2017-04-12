#####################################################################################
# File: 	Examples.R
# Project:  Practical Tools 2-day short course
# Date: 	01/17/2014
# Author:	R. Valliant, J.A. Dever
# Purpose:	Examples to be used in class
#           More extensive example code is available on the website
#####################################################################################

require(PracTools)

# Example 3.1.
# We estimate from a previous survey that population
# CV of some variable is 2.0. If pop is extremely large and
# CV0=0.05, use

nCont(CV0=0.05, CVpop=2)

# Example 3.4
# Suppose pU = 0.01. If we require a CV of 0.05, the
# standard error of the proportion would be 0.0005. The sample
# size is 39,600 and is computed with

nProp(V0=0.0005^2, N=Inf, pU=0.01)
nProp(CV0=0.05, N=Inf, pU=0.01)

#--------------------------------------------------------------------------------------------------

# Example 3.6
# Estimate a proportion where pU = 0:5 with a margin of error (MOE) of e when  = 0:05.
# The sample should be large # of error (MOE) of e when  = 0.05. The sample should be large
# enough that a normal approximation 95% CI should be 0:50 +/- e. For example, if e = 0:03
# and p were actually 0.5, we want the CI to be 0.50 +/- 0.03 = [0.47, 0.53].
# R function to compute sample sizes with pU = 0:5 with an MOE equal to half-width of a CI (moe.sw=1)
# to implement (3.10)

nPropMoe(moe.sw=1, e=seq(0.01,0.08,0.01),alpha=0.05, pU=0.5)

#--------------------------------------------------------------------------------------------------

# Example 3.9
# Find allocation to minimize var of mean for a total budget of $100,000

ch <- c(1400, 200, 300, 600, 450, 1000)
Nh <- c(215, 65, 252, 50, 149, 144)
Sh <- c(2678, 1064, 690, 1108, 981, 4455)
alloc <- strAlloc(Nh = Nh, Sh = Sh, cost = 100000, ch = ch,alloc = "totcost")
alloc
round(alloc$nh,1)
round(alloc$'nh/n',2)

#--------------------------------------------------------------------------------------------------
# Example 3.11
# Estimate gamma variance parameter

data(hospital)
sam <- c(7, 17, 30, 33, 62, 111, 139, 247, 370, 393)

x <- hospital$x[sam]
y <- hospital$y[sam]


X <- cbind(sqrt(x), x)
gammaFit(X = X, x = x, y = y, maxiter=100, tol=0.001)

#--------------------------------------------------------------------------------------------------

# Example 4.6
# Two domains (males and females) and want to have equal size
# samples of men and women large enough to detect a difference in
# mean weights of 5 kg with power 0.80. We estimate that sigma^2(M)=
# sigma^2(F)=200. Thus, sd in the input to power.t.test is sqrt(200).
# To do a 1-sided 0.05 level test with power of 0.80, use

power.t.test(power = 0.8, delta = 5, sd = sqrt(200),
    type = "two.sample", alt = "one.sided", sig.level = 0.05)


#--------------------------------------------------------------------------------------------------

# Example 9.2 Variance components in srs/srs
data(MDarea.pop)
BW2stageSRS(MDarea.pop$y1, psuID=MDarea.pop$PSU)

#--------------------------------------------------------------------------------------------------

# Example 9.4 Variance components in ppswr/srs
pp.PSU <- table(MDarea.pop$PSU) / nrow(MDarea.pop)
BW2stagePPS(MDarea.pop$y1, pp=pp.PSU, psuID=MDarea.pop$PSU)

pp.SSU <- table(MDarea.pop$SSU) / nrow(MDarea.pop)
BW2stagePPS(MDarea.pop$y1, pp=pp.SSU, psuID=MDarea.pop$SSU)

pp.TRACT <- table(MDarea.pop$TRACT) / nrow(MDarea.pop)
BW2stagePPS(MDarea.pop$y1, pp=pp.TRACT, psuID=MDarea.pop$TRACT)

#--------------------------------------------------------------------------------------------------

# Example 9.7
# Compute optimal no. of PSUs and elements per PSU assuming
# that C1 = 750, C2 = 100, delta = 0.05, unit relvariance and k are 1,
# total budget for variable costs is $100,000.

clusOpt2(C1=750, C2=100, delta=0.05, unit.rv=1, k=1,tot.cost=100000, cal.sw=1)

#--------------------------------------------------------------------------------------------------

# Example 9.11 Estimated variance components in 2-stage sampling
require(sampling)
require(reshape) # has function that allows renaming variables
Ni <- table(MDarea.pop$TRACT)
m <- 20
probi <- m*Ni / sum(Ni)
    # select sample of clusters
set.seed(-780087528)
sam <- cluster(data=MDarea.pop, clustername="TRACT",
            size=m, method="systematic",
            pik=probi, description=TRUE)
    # extract data for the sample clusters
samclus <- getdata(MDarea.pop, sam)
samclus <- rename(samclus, c(Prob = "pi1"))
    # treat sample clusters as strata and select
    # srswor from each
s <- strata(data = as.data.frame(samclus),
    stratanames = "TRACT",
    size = rep(50,m), method="srswor")
    # extracts the observed data
samdat <- getdata(samclus,s)
samdat <- rename(samdat, c(Prob = "pi2"))
    # extract pop counts for PSUs in sample
pick <- names(Ni) %in% sort(unique(samdat$TRACT))
Ni.sam <- Ni[pick]
pp <- Ni.sam / sum(Ni)
wt <- 1/samdat$pi1/samdat$pi2

BW2stagePPSe(Ni = Ni.sam, ni = rep(50,20),
    X = samdat$y1, psuID = samdat$TRACT,
    w = wt, m = 20, pp = pp)

#--------------------------------------------------------------------------------------------------

# Example 13.6 Fit a propensity model
data(nhis)
glm.logit <- glm(resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           family=binomial(link = "logit"),
           data = nhis)
summary(glm.logit)
L.hat <- glm.logit$linear.predictors
                    # transform link values to probability scale
pred.logit <- exp(L.hat) / (1 + exp(L.hat) )

#--------------------------------------------------------------------------------------------------

# Example 13.8 Form classes based on propensities and check balance
quintiles <- quantile(pred.logit, probs = seq(0,1,0.2))
    # Create a factor to hold the class IDs
    # include.lowest=TRUE makes sure the smallest
    # propensity is assigned to a class
p.class <- cut(round(pred.logit,3), breaks = quintiles,
    include.lowest=TRUE)
table(p.class)
#[0.453,0.631] (0.631,0.677] (0.677,0.714] (0.714,0.752] (0.752,0.818]
#          778           773           788           786           786

chk1 <- glm(age ~ p.class + resp + p.class*resp, data = nhis)
summary(chk1)

    # Use pclass fcn in PracTools v.0.1
    # Note that pclass does not use rounded predictions ==> counts by class differ from ones in book
data(nhis)
out <- pclass(formula = resp ~ age + as.factor(hisp) + as.factor(race) + as.factor(parents_r) +
           as.factor(educ_r),
            data = nhis, type = "unwtd", link="logit", numcl=5)
table(out$p.class)
#[0.453,0.631] (0.631,0.677] (0.677,0.714] (0.714,0.752] (0.752,0.818]
#          790           784           775           780           782


#--------------------------------------------------------------------------------------------------

# Example 14.2 Poststratification
# Select a simple random sample of size 250 from the large NHIS population
# Poststrata are defined by age group crossed with Hispanicity.
# Create single variable to identify age.grp x hisp.r poststrata

require(survey)

        # collapse hisp = 3,4
data(nhis.large)
hisp.r <- nhis.large$hisp
hisp.r[nhis.large$hisp ==4] <- 3
table(hisp.r)
nhis.large1 <- data.frame(nhis.large, hisp.r)

m <- max(nhis.large1$hisp.r)
nhis.large1$PS <- (nhis.large1$age.grp - 1)*m + nhis.large1$hisp.r
N.PS <- table(PS = nhis.large1$PS)
    # select srswor of size n
set.seed(-1570723087)
n <- 250
N <- nrow(nhis.large1)
sam <- sample(1:N, n)
samdat <- nhis.large1[sam, ]

    # compute srs weights and sampling fraction
d <- rep(N/n, n)
f1 <- rep(n/N, n)
nhis.dsgn <- svydesign(ids = ~0,    # no clusters
          strata = NULL,    # no strata
          fpc = ~f1,
          data = data.frame(samdat),
          weights = ~d)

        # poststratified design object
ps.dsgn <- postStratify(design = nhis.dsgn,
                        strata = ~PS,
                        population = N.PS)

svytotal(~as.factor(PS), ps.dsgn)

    # PS standard errors and cv’s
svytotal(~ as.factor(medicaid), ps.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), ps.dsgn, na.rm=TRUE))
    # srs standard error and cv’s
svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE))

#--------------------------------------------------------------------------------------------------

# Example 14.4 Raking
# Create marginal pop totals
N.age <- table(nhis.large1$age.grp)
N.hisp <- table(nhis.large1$hisp.r)
pop.totals <- c('(Intercept)' = N, N.age[-1], N.hisp[-1])
    # create raked weights
rake.dsgn <- calibrate(design = nhis.dsgn,
    formula = ~as.factor(age.grp) + as.factor(hisp.r),
    calfun = "raking",
    population = pop.totals)
    # raking standard errors and cv’s
svytotal(~ as.factor(medicaid), rake.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), rake.dsgn, na.rm=TRUE))

#--------------------------------------------------------------------------------------------------
#   GREG example

require(sampling)
data(smho.N874)
    # delete hospital type 4 (outpatient, partial care)
delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]
dim(smho)

    # recode small hospitals to have a minimum MOS
x <- smho[,"BEDS"]
x[x <= 5] <- 5
x <- sqrt(x); n <- 80
set.seed(428274453)
pk <- n*x/sum(x)
    # select systematic pps sample after sorting pop in random order
sam <- UPrandomsystematic(pk)
sam <- sam==1
sam.dat <- smho[sam, ]
d <- 1/pk[sam]

    # Create design object that is used in calibrate function to compute GREG weights.
smho.dsgn <- svydesign(ids = ~0, # no clusters
                strata = NULL, # no strata
                data = data.frame(sam.dat),
                weights = ~d)
    # Compute pop totals of auxiliaries
    # Note these are the original not the recoded x’s
x.beds <- by(smho$BEDS, smho$hosp.type, sum)
x.seen <- sum(smho$SEENCNT)
x.eoy <- sum(smho$EOYCNT)
N <- nrow(smho)
pop.tots <- c('(Intercept)' = N,
                SEENCNT = x.seen,
                EOYCNT = x.eoy,
                x.beds = x.beds)
    # Compute GREG weights with control on beds within hospital type but without hospital type controls
sam.lin <- calibrate(design = smho.dsgn,
            formula = ~SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
            population = pop.tots,
            calfun="linear")

svytotal(~SEENCNT, sam.lin)
svytotal(~EOYCNT, sam.lin)

    # examine weights
summary(weights(smho.dsgn))
summary(weights(sam.lin))

    # bound the weight adjustments
sam.linBD <- calibrate(design = smho.dsgn,
                formula = ~SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
                population = pop.tots,
                bounds = c(0.4, 3),
                calfun = "linear")
summary(weights(sam.linBD))

    # estimates with different sets of weights
svytotal(~EXPTOTAL, design=smho.dsgn)
cv(svytotal(~EXPTOTAL, design=smho.dsgn))

svytotal(~EXPTOTAL, design=sam.lin)
cv(svytotal(~EXPTOTAL, design=sam.lin))

svytotal(~EXPTOTAL, design=sam.linBD)
cv(svytotal(~EXPTOTAL, design=sam.linBD))

    # GREG weights using BEDS as quantitative covariate and hosp.type as factor
x.beds <- sum(smho$BEDS)
N.hosp <- table(smho$hosp.type)
pop.tots <- c(BEDS = x.beds,
              SEENCNT = x.seen,
              EOYCNT = x.eoy,
              HOSP = N.hosp
              )
sam.lin2 <- calibrate(design = smho.dsgn,
                formula = ~ 0 + BEDS + SEENCNT + EOYCNT + as.factor(hosp.type),
                population = pop.tots,
                bounds=c(-Inf,Inf),
                calfun="linear")
    # compare estimates of direct financing with two sets of GREG weights
svymean(~ as.factor(FINDIRCT), sam.lin)
cv(svymean(~ as.factor(FINDIRCT), sam.lin))

svymean(~ as.factor(FINDIRCT), sam.lin2)
cv(svymean(~ as.factor(FINDIRCT), sam.lin2))


#--------------------------------------------------------------------------------------------------

# Example 15.5 Effect of using fpc’s

require(survey)
require(sampling)
    # Population stratum counts
Nh <- table(smho.N874[, "hosp.type"])
    # Select a stratified simple random sample within hospital type strata
set.seed(428274453)
n <- 50
H <- length(Nh)
sam <- strata(data = smho.N874,
            stratanames = "hosp.type",
            size = rep(n,H), method=c("srswor"),
            description = TRUE)
sam.dat <- smho.N874[sam$ID_unit,]

d <- 1/sam$Prob
sam.rates <- sam$Prob
    # Create a design object with fpc’s
smho.dsgn <- svydesign(ids = ~0, # no clusters
                strata = ~hosp.type,
                fpc = ~sam.rates,
                data = data.frame(sam.dat),
                weights = ~d)
cv(svyby(~EXPTOTAL, by=~as.factor(hosp.type),
        design=smho.dsgn, FUN=svytotal))
cv(svytotal(~EXPTOTAL, design=smho.dsgn))

#--------------------------------------------------------------------------------------------------

#  Example 15.7 Linearization accounting for poststratification
    # collapse hisp = 3,4
hisp.r <- nhis.large$hisp
hisp.r[nhis.large$hisp == 4] <- 3
nhis.large1 <- data.frame(nhis.large, hisp.r)
    # create single variable to identify
    # age.grp x hisp.r poststrata
m <- max(nhis.large1$hisp.r)
nhis.large1$PS <- (nhis.large1$age.grp - 1)*m + nhis.large1$hisp.r
N.PS <- table(PS = nhis.large1$PS)
ps.dsgn <- postStratify(design = nhis.dsgn,
            strata = ~PS,
            population = N.PS)
svytotal(~ as.factor(medicaid), ps.dsgn, na.rm=TRUE)
wts <- weights(ps.dsgn)
    # design object ignoring PS
noPS.dsgn <- svydesign(ids = ~0,
                strata = NULL,
                data = data.frame(samdat),
                weights = ~wts)
svytotal(~ as.factor(medicaid), noPS.dsgn, na.rm=TRUE)

#--------------------------------------------------------------------------------------------------

#  Example 15.9 Jackknife
        # create a design object
nhis.dsgn <- svydesign(ids = ~psu,
                         strata = ~stratum,
                         nest = TRUE,           # clusters are renumbered within PSUs
                         data = nhis.large,
                         weights = ~svywt)
        # JKn
jkn.dsgn <- as.svrepdesign(design = nhis.dsgn, type = "JKn")

        # 1-way table
a <- svymean(~factor(age.grp), deff=TRUE, design=jkn.dsgn)
b <- ftable(a, rownames = list(age = c("< 18", "18-24", "25-44", "45-64", "65+")))
round(b,4)

#--------------------------------------------------------------------------------------------------

#  Example 15.11 Jackknife with poststratification
#  This example uses nhis.large1 that was created above
#  N.PS from above is used for poststratum pop counts

        # select  srswor of size n
set.seed(-1570723087)
n <- 250
N <- nrow(nhis.large1)
sam <- sample(1:N, n)
samdat <- nhis.large1[sam, ]
    # check sample counts in each PS
n.PS <- table(samdat[, "age.grp"], samdat[, "hisp.r"])
as.vector(n.PS)

        # compute srs weights and sampling fraction
d <- rep(N/n, n)
        # srswor design object
nhis.dsgn <- svydesign(ids = ~0,
          strata = NULL,
          data = data.frame(samdat),
          weights = ~d)

jk1.dsgn <- as.svrepdesign(design = nhis.dsgn, type = "JK1")

        # poststratified design object
jk1.ps.dsgn <- postStratify(design = jk1.dsgn,
                        strata = ~PS,
                        population = N.PS)

        # Check that weights are calibrated for x's
svytotal(~ as.factor(PS), jk1.ps.dsgn)

        # PS standard errors and cv's
svytotal(~ as.factor(medicaid), jk1.ps.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), jk1.ps.dsgn, na.rm=TRUE))

#--------------------------------------------------------------------------------------------------

#   Example 15.14 Bootstrap

x <- smho.N874$BEDS
x[x <= 10] <- 10
x <- sqrt(x)
smho.N874 <- smho.N874[order(x), ]
x <- sort(x)

N <- nrow(smho.N874)
n <- 50
H <- n/2

    # create strata based on cumulative sqrt(x)
cumx <- cumsum(x)
size <- cumx[N]/H
brks <- (0:H)*size
strat <- cut(cumx, breaks = brks, labels = 1:H)

pop <- data.frame(smho.N874, strat = strat)

        # stsrswor from strata based on a measure of size
set.seed(428274453)
sam <- strata(data = pop,
              stratanames = "strat",
              size = rep(2,H), method=c("srswor"))

sam.dat <- pop[sam$ID_unit,]
d <- 1/sam$Prob

smho.dsgn <- svydesign(ids = ~0,
                       strata = ~strat,
                       data = sam.dat,
                       fpc = sam$Prob,
                       weights = ~d)

        # create design with boostrap wts.
        # Rao-Wu version used with mh = nh-1
smho.boota <- as.svrepdesign(design = smho.dsgn,
                             type = "subbootstrap",
                             replicates = 500)

        # mean & CI for EOYCNT based on RW bootstrap
a1 <- svytotal(~EOYCNT, design = smho.boota,
        na.rm=TRUE,
        return.replicates = TRUE)
a1

             # Compute CI based on bootstrap percentile method.
ta1 <- quantile(a1$replicates, c(0.025, 0.975))
ta1

         # t approximation with v.boot
La <- a1$mean + qt(0.025,df=degf(smho.boota))*sd(a1$replicates)
Ua <- a1$mean + qt(0.975,df=degf(smho.boota))*sd(a1$replicates)
c(La[1], Ua[1])
        # compare to confint which uses normal approximation
confint(a1)

#--------------------------------------------------------------------------------------------------
