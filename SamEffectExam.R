#Design-based t-test
library(survey)
library(sampling)


select.apiTwoStage <- 
  function(n_I, n_c, pop){
    apipop_mss <- pop
    h_c  <- table(apipop_mss$cnum)/nrow(apipop_mss)*n_I
    
    
    p_c  <- h_c%%1
    d_c  <- h_c%/%1
    s_c  <- UPmaxentropy(p_c)
    
    n_i  <- (s_c+d_c)*n_c
    
    s.snum <- unlist(
      mapply(function(x,y) sample(as.character(x), y), 
             tapply(apipop_mss$snum,apipop_mss$cnum,identity)[names(n_i)],
             n_i
      )
    )
    
    h_ci  <- h_c[apipop_mss$cnum]
    n_i.m <- rep(n_c,length(h_ci))
    
    n_i.m[h_ci>1] <- h_ci[h_ci>1]*n_c
    
    p_ci         <- h_ci
    p_ci[h_ci>1] <- 1
    
    apipop_mss$fpc2 <- n_i.m[apipop_mss$cnum]/table(apipop_mss$cnum)[as.character(apipop_mss$cnum)]
    apipop_mss$fpc1 <- p_ci[as.character(apipop_mss$cnum)]
    
    
    apiclus3 <- apipop_mss[apipop_mss$snum%in%as.numeric(s.snum),]
    
    svydesign(id=~cnum+snum, fpc = ~fpc1 + fpc2, data=apiclus3, pps = "brewer")
    
    
    
  }



select.apiClu <- 
  function(n_I){
    
    s.snum <- sample(as.character(apipop_mss$dnum), n_I)
        
    apipop_mss$fpc1 <- length(unique(apipop_mss$dnum))
    
    apiclus         <- apipop_mss[apipop_mss$dnum %in% as.numeric(s.snum), ]
    
    svydesign(id=~dnum, fpc = ~fpc1, data=apiclus)
    
  }



#design object for rotational panels

#select.apiSRS
#select.apiStrSRS
#select.apiStrTwoStag

#Two independent Samples
set.seed(1341)

tv <- mean(apipop$api00-apipop$api99)

mu_Ho <- tv
apipop_mss <- apipop
#apipop_mss$api00 <- apipop_mss$api99+rnorm(nrow(apipop_mss),0,sd(apipop_mss$api99)^{1/2})


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
  var(dclus_1$variables$api00)/nrow(dclus_1$variables)*(1-nrow(dclus_1$variables)/6194) + 
  var(dclus_2$variables$api00)/nrow(dclus_2$variables)*(1-nrow(dclus_2$variables)/6194)


svyConf.sim <- c(delta + qnorm(1-0.975)*sqrt(var.delta.sim ),
                 delta + qnorm(0.975)*sqrt(var.delta.sim )
)

# simConf <- t.test(x = dclus_2$variables$api00, 
#                   y = dclus_2$variables$api99, 
#                   var.equal = FALSE, 
#                   mu = mu_Ho)

simH0    <- svyConf.sim[1] < mu_Ho & svyConf.sim[2] > mu_Ho

#simH0    <- simConf$p.value < 0.05

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


