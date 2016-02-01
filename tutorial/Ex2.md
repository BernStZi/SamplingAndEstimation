Exercise 2
----------

1 Load the survey package and the api datasets in it.

2 The dataset apistrat is a sample of schools from apipop stratified by
stype. Assuming the selection within the strata was done by SRS, define
a svydesign object that enables your to make unbiased point and variance
estimates. Estimate the mean of api00.

3 Now you should try different allocations. Using stype again as a
stratification variable calculate the allocation of a sample of 60
schools from apipop, using equal, proportional to the number of schools,
and optimal with regard to api99 allocation.

4 Select a StrSRS from apipop for each of your allocations.

5 Estimate again the mean of api00 from your three different samples.

The `survey` library
--------------------

-   Load `survey` library and dataset apistrat

The survey library
------------------

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

Exercise 1 - Part 2
-------------------

Assuming the selection within the strata was done by SRS, define a
svydesign object that enables you to make unbiased point and variance
estimates.

-   Estimate the mean of variable `api00`.

<!-- -->

    mean(apistrat$api00)

    ## [1] 652.82

Exercise 1 - Part 3
-------------------

Now you should try different allocations.

Using `stype` as a stratification variable calculate the allocation of a
sample of 60 schools from apipop. Use

-   equal allocation
-   proportional allocation (proportional to nr. of schools)
-   optimal allocation (with regard to `api99` allocation)

Select a StrSRS from apipop for each of your allocations.

-   Estimate again the mean of api00 from your three different samples.

Exercise 2 - Estimation under stratified design
-----------------------------------------------

-   Download ESS
-   Define a survey object

Exercise 1 - Part 1
-------------------

Download ESS

-   Download the ESS dataset for
    [Denmark](http://www.europeansocialsurvey.org/file/download?f=ESS5DK.spss.zip&c=DK&y=2010)
    (Sampling Data and Country File) of the 5th round

Packages for data import
------------------------

-   Use the package `foreign`

<!-- -->

    library(foreign)

    library(memisc)

[Import portable spss-files](http://stackoverflow.com/questions/3136293/read-spss-file-into-r)
----------------------------------------------------------------------------------------------

    DK <- as.data.set(spss.portable.file("ESS5DK.por"))
    SE <- as.data.set(spss.portable.file("ESS5SE.por"))

    DK <- as.data.frame(DK)
    SE <- as.data.frame(SE)

Load the ESS dataset and the country file
-----------------------------------------
