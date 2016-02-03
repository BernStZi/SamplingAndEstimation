\name{strSRsample}
\alias{strSRsample}
\title{Draw a stratified sample}
\usage{
strSRsample()
}
\description{
Draw a stratified sample
}
\examples{
library(survey)
data(api)

nh.eq <- c(20,20,20)
names(nh.eq) <- names(table(apipop$stype))

s.eq <- strSRsample(apipop$stype, nh.eq, replace=FALSE)
}
