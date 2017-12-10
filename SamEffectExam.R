#Design-based t-test
library(survey)
library(sampling)


select.apiTwoStage <- 
  function(n_I, n_c, pop){
    data(api)
    dat <- apipop
    h_c  <- table(dat$cnum)/nrow(dat)*n_I
    
    
    p_c  <- h_c%%1
    d_c  <- h_c%/%1
    s_c  <- UPmaxentropy(p_c)
    
    n_i  <- (s_c+d_c)*n_c
    
    s.snum <- unlist(
      mapply(function(x,y) sample(as.character(x), y), 
             tapply(dat$snum,dat$cnum,identity)[names(n_i)],
             n_i
      )
    )
    
    h_ci  <- h_c[as.character(dat$cnum)]
    n_i.m <- rep(n_c,length(h_ci))
    
    n_i.m[h_ci>1] <- h_ci[h_ci>1]*n_c
    
    p_ci         <- h_ci
    p_ci[h_ci>1] <- 1
    
    dat$fpc2 <- n_i.m[dat$cnum]/table(dat$cnum)[as.character(dat$cnum)]
    dat$fpc1 <- p_ci[as.character(dat$cnum)]
    
    
    apiclus3 <- dat[dat$snum%in%as.numeric(s.snum),]
    
    svydesign(id=~cnum+snum, fpc = ~fpc1 + fpc2, data=apiclus3, pps = "brewer")
    
    
    
  }



select.apiClu <- 
  function(n_I){
    data(api)
    dat <- apipop
    s.dnum <- sample(as.character(dat$dnum), n_I)
        
    dat$fpc1 <- length(unique(dat$dnum))
    
    apiclus         <- dat[dat$dnum %in% as.numeric(s.dnum), ]
    
    svydesign(id=~dnum, fpc = ~fpc1, data=apiclus)
    
  }

rand.round <- function(x){
  
  int <- x%/%1
  round.int <- sampling::UPbrewer(x%%1)
  rannum <- runif(length(x))
  int+round.int
  
}

select.apiStra <- 
  function(n){
    #prob allocation
    data(api)
    dat <- apipop
    nh_ <- rand.round(table(dat$cnum)/nrow(dat)*n)
    nh  <- nh_
    nh[nh_<2] <- 2
    
    s.snum <- unlist(
      mapply(function(x,y) sample(as.character(x), y), 
             tapply(dat$snum,dat$cnum,identity)[names(nh)],
             nh
      )
    )
    
    dat$fpc1 <- table(dat$cnum)[as.character(dat$cnum)]
    apistra  <- dat[dat$snum %in% as.numeric(s.snum), ]
    
    svydesign(id=~snum, fpc = ~fpc1, strata = ~cnum, data=apistra)
    
  }


#design object for rotational panels

#select.apiSRS
#select.apiStrSRS
#select.apiStrTwoStag

#Two independent Samples
set.seed(1341)

tv <- mean(apipop$api00-apipop$api99)

mu_Ho <- tv

#dat$api00 <- dat$api99+rnorm(nrow(dat),0,sd(dat$api99)^{1/2})


OUT <- 
sapply(1:1000,function(x){

#dclus_1 <- select.apiTwoStage(20,2)
#dclus_2 <- select.apiTwoStage(20,2)
  
dclus_1 <- select.apiClu(25)
dclus_2 <- select.apiClu(25)
 

var.delta <- SE(svymean(~api00,dclus_1))^2 + SE(svymean(~api99,dclus_2))^2
delta     <- as.vector(svymean(~api00,dclus_2))-as.vector(svymean(~api99,dclus_1))

svyConf.int <- c(delta + qnorm(1-0.975)*sqrt(var.delta),
                 delta + qnorm(0.975)*sqrt(var.delta)
                 )

svyH0    <- svyConf.int[1] < mu_Ho & svyConf.int[2] > mu_Ho
#svyH0   <- !svyConf.int[1] < 0 & svyConf.int[2]>0


var.delta.sim <- 
  var(dclus_1$variables$api00)/nrow(dclus_1$variables)+ 
  var(dclus_2$variables$api00)/nrow(dclus_2$variables)


svyConf.sim <- c(delta + qnorm(1-0.975)*sqrt(var.delta.sim ),
                 delta + qnorm(0.975)*sqrt(var.delta.sim )
)

# simConf <- t.test(x = dclus_2$variables$api00, 
#                   y = dclus_2$variables$api99, 
#                   var.equal = FALSE, 
#                   mu = mu_Ho)
#simH0    <- simConf$p.value < 0.05

simH0    <- svyConf.sim[1] < mu_Ho & svyConf.sim[2] > mu_Ho

c(svyH0, simH0, delta)


})



#other way arround with stratification...

#does the confidence 

#two sample test

data(api)
dclus2 <- svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)
svyttest(enroll~comp.imp, dclus2)


svymean(~api00,dclus2)-svymean(~api99,dclus2)

svyttest(I(api00-api99)~0, dclus2)

t.test(x=dclus2$variables$api00, y=dclus2$variables$api99)

###

summary(lm(api00~ell+meals+mobility+stype, data = dclus3$variables, weights = weights(dclus3)))
summary(svyglm(api00~ell+meals+mobility+stype, design=dclus3))

###SRS
#stratified by 
apipop_srs <- apipop

n      <- 20
s.snum <- sample(as.character(apipop$snum), 20)


apipop_srs$fpc <- nrow(apipop_srs)

apisrs2 <- apipop_srs[apipop_srs$snum%in%as.numeric(s.snum),]

dsrs2 <- svydesign(id=~snum, fpc = ~fpc, data=apisrs2)

summary(svyglm(api00~ell+meals+mobility, design=dsrs2))
summary(svyglm(api00~ell+meals+mobility, design=dclus3))


###StrSRS
apipop_StrSRS <- apipop
table(apipop_StrSRS$stype)/nrow(apipop_StrSRS)*20


20

summary(svyglm(api00~ell+meals+mobility, design=dsrs2))
summary(svyglm(api00~ell+meals+mobility, design=dclus3))

###Str MSS
#use prob argument for equal probs

summary(svyglm(api00~ell+meals+mobility, design=rstrat))
summary(svyglm(api00~ell+meals+mobility, design=rclus2))


