Exercise 2.A
------------

Estimation under a stratified design

-   [Download](https://github.com/BernStZi/SamplingAndEstimation/blob/short/tutorial/preparation/Preparation.md)
    the ESS for
    [Sweden](http://www.europeansocialsurvey.org/file/download?f=ESS7SE.spss.zip&c=SE&y=2010)
    and
    [Denmark](http://www.europeansocialsurvey.org/file/download?f=ESS5DK.spss.zip&c=DK&y=2010)
    (round 5)
-   Import data to R and combine the two datasets
-   Define a `survey` object (stratified design)
-   Calculate the combined total (nr. of persons watching 3 or more
    hours) for the tv consumption (`tvtot`) and compare it with the
    totals in Sweden and Denmark

[Codebook ESS round
5](http://www.europeansocialsurvey.org/docs/round5/survey/ESS5_appendix_a6_e04_0.pdf)

Exercise 2.B
------------

-   Load the `survey` package and the `api` datasets.

<!-- -->

    library(survey)
    data(api)

-   The dataset `apistrat` is a sample of schools from `apipop`
    stratified by `stype`. Assuming the selection within the strata was
    done by SRS, define a survey object (`svydesign`) and calculate a
    point and variance estimate for the mean of `api00`.

Exercise 2.B
------------

-   Using `stype` again as a stratification variable try different
    allocations for stratified sample. Calculate the allocation of a
    sample of 60 schools from `apipop` using equal, proportional and
    optimal allocation. The proportional allocation should be
    proportional to the number of schools within the strata and the
    optimal alloaction should be optimal with regard to `api99`.

Exercise 2.B
------------

-   Select a StrSRS from `apipop` for each allocation.
-   Estimate again the mean of `api00` from all three samples and
    compare the results.

Function for stratified samples
-------------------------------

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

Getting the function
--------------------

    library(devtools)
    install_github("BernStZi/SamplingAndEstimation/r/sampaest",
                   ref="short")

    url <- "https://raw.githubusercontent.com/BernStZi/
    SamplingAndEstimation/short/r/sampaest/R/strSRsample.R"
    source(url)
