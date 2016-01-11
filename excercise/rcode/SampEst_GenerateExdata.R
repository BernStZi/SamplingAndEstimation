#--------------------------------#
# Sampling and Extimation
# Jan-Philipp Kolb, Matthias Sand and Stefan Zins
# Day 1 - Generating an example dataset
#--------------------------------#


id <- 1:10000
set.seed(42)
education <- sample(c("none","low","average","high"),10000, 
                    replace = T,prob = c(.072,.356,.289,.283))

gender <- sample(c("male","female"),10000,
                 replace = T,prob = c(.488,.512))

iq <- rnorm(10000,100,20)
my.pop <- data.frame(id,gender,education,iq)