Example A
---------

Estimation for the European Social Survey under stratified design

-   Download ESS for Sweden and Denmark
-   Import data to R and merge the two datasets
-   Define a `survey` object (stratified design)
-   Calculate the unbiased totals for the tv consumption

Download and Import ESS
-----------------------

-   Download the ESS dataset for
    [Denmark](http://www.europeansocialsurvey.org/file/download?f=ESS5DK.spss.zip&c=DK&y=2010)
    (Sampling Data and Country File) of the 5th round

Packages for data import

-   Use the package `foreign` or `memisc` for import

<!-- -->

    library(foreign)

    library(memisc)

Load the ESS dataset and the country file
-----------------------------------------

    library(foreign)
    DK <- read.spss("ESS5DK.sav",to.data.frame=T)
    SE <- read.spss("ESS5SE.sav",to.data.frame=T)

    DK <- as.data.frame(DK)
    DK$N <- DK$pweight*10000*nrow(DK)

    SE <- as.data.frame(SE)
    SE$N <- SE$pweight*10000*nrow(SE)

    DK_tv <- data.frame(tvtot=as.character(DK$tvtot),
                        N=DK$N,
                        cntry=as.character(DK$cntry))
    SE_tv <- data.frame(tvtot=as.character(SE$tvtot),
                        N=SE$N,
                        cntry=as.character(SE$cntry))


    NE <- rbind(DK_tv,SE_tv)

Define a survey object
----------------------

    library(survey)

[Define a survey
object:](http://r-survey.r-forge.r-project.org/survey/example-design.html)

    svydes_NE <- svydesign(id=~1,strata=~cntry, fpc=~N, data=NE)

    svytable(~tvtot,svydes_NE)

Example B
---------

-   Load the survey package and the `api` datasets.

-   Compute the mean of the Academic Performance Index (2000), asuming
    SRS

-   Use other allocations

-   Select a StrSRS from apipop for each allocations.

-   Estimate the mean of api00 from different samples (equal,
    proportional, optimal).

The survey library
------------------

Load `survey` library and dataset apistrat

    library(survey)

The dataset apistrat is a sample of schools from apipop stratified by
stype.

    data(api)

    head(apistrat)

<table>
<thead>
<tr class="header">
<th align="left">cds</th>
<th align="left">stype</th>
<th align="left">name</th>
<th align="left">sname</th>
<th align="right">snum</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">19647336097927</td>
<td align="left">E</td>
<td align="left">Open Magnet: Ce</td>
<td align="left">Open Magnet: Center for Individual (Char</td>
<td align="right">2077</td>
</tr>
<tr class="even">
<td align="left">19647336016018</td>
<td align="left">E</td>
<td align="left">Belvedere Eleme</td>
<td align="left">Belvedere Elementary</td>
<td align="right">1622</td>
</tr>
<tr class="odd">
<td align="left">19648816021505</td>
<td align="left">E</td>
<td align="left">Altadena Elemen</td>
<td align="left">Altadena Elementary</td>
<td align="right">2236</td>
</tr>
<tr class="even">
<td align="left">19647336019285</td>
<td align="left">E</td>
<td align="left">Soto Street Ele</td>
<td align="left">Soto Street Elementary</td>
<td align="right">1921</td>
</tr>
<tr class="odd">
<td align="left">56739406115430</td>
<td align="left">E</td>
<td align="left">Walnut Canyon E</td>
<td align="left">Walnut Canyon Elementary</td>
<td align="right">6140</td>
</tr>
<tr class="even">
<td align="left">56726036084917</td>
<td align="left">E</td>
<td align="left">Atherwood Eleme</td>
<td align="left">Atherwood Elementary</td>
<td align="right">6077</td>
</tr>
<tr class="odd">
<td align="left">56726036055800</td>
<td align="left">E</td>
<td align="left">Township Elemen</td>
<td align="left">Township Elementary</td>
<td align="right">6071</td>
</tr>
<tr class="even">
<td align="left">15633216109078</td>
<td align="left">E</td>
<td align="left">Thorner (Dr. Ju</td>
<td align="left">Thorner (Dr. Juliet) Elementary</td>
<td align="right">904</td>
</tr>
</tbody>
</table>

Stratified designs
------------------

Assuming the selection within the strata was done by SRS, define a
svydesign object that enables you to make unbiased point and variance
estimates.

-   Estimate the mean of variable `api00`.

<!-- -->

    mean(apistrat$api00)

    ## [1] 652.82

Allocations
-----------

Now you should try different allocations.

Using `stype` as a stratification variable calculate the allocation of a
sample of 60 schools from apipop. Use

-   equal allocation
-   proportional allocation (proportional to nr. of schools)
-   optimal allocation (with regard to `api99` allocation)

Select a StrSRS from apipop for each of your allocations.

Equal allocation
----------------

    library(sampling)
    Nh <- table(apistrat$stype)

    s_equal <- strata(apistrat,"stype",
             size=c(20,20,20), 
             method="srswor")

    ind <- match(s_equal$stype,names(Nh))
    s_equal$N <- Nh[ind]
    s_equal$api00 <- apistrat$api00[s_equal$ID_unit]

Proportional allocation
-----------------------

    strSRsample <- function(strind, nh, replace=FALSE){
      Nh <- table(strind)[names(nh)]
      h.id <- split(1:sum(Nh), strind)[names(nh)]
      
      
      sam <- mapply( function(x,y) sample(x, y, replace=replace)
                     , Nh, nh, SIMPLIFY = F)
      unlist(mapply(function(x,y) x[y]
                    , h.id
                    , sam, SIMPLIFY = F)
             ,use.names = FALSE)
    }

    n <- 2000

    nh <- tapply(apipop$api99,apipop$stype,function(x)sum(x)/sum(apipop$api99)*n)
    samp <- strSRsample(apipop$stype,nh)

    ap <- apipop[samp,]

Estimation
----------

-   Estimate again the mean of api00 from your three different samples.

<!-- -->

    svy_equal <- svydesign(id=~1,strata=~stype, fpc=~N, data=s_equal)

    svymean(s_equal$api00,svy_equal)

    ##        mean     SE
    ## [1,] 680.19 13.105

    library(devtools)

    install_github("BernStZi/SamplingAndEstimation/r/sampaest",ref="short")
