#################################
## Setting the Directory ########
#################################

path <- "G:/Sand_Summerschool/Excersises/"
setwd(path)

#########################
## Excersise 1###########
#########################
?rexp
N <- 1000
set.seed(42)
num <- rexp(N)

n1 <-2
n2 <- 10
n3 <- 100

set.seed(42)
s1 <- sample(num,n1)
s2 <- sample(num,n2)
s3 <- sample(num,n3)

pdf("density2.pdf")
par(mfrow=c(1,1))



plot(s1)
abline(v=mean(s1),col=3)

plot(density(s2),type="l",col="blue",main="n2=10")
abline(v=mean(s2),col="blue")

plot(density(s3),main="n3=100")
abline(v=mean(s3))
dev.off()





#########################
## Excersise 2###########
#########################

library(sampling)
data("belgianmunicipalities")
bm <- belgianmunicipalities

str(bm)

## Mean and Variance of bm

mean(bm$averageincome)

var(bm$averageincome) # Wrong! -> see variance decomp.

## by Province

s.mean <- vector()

s.mean[1] <- mean(bm$averageincome[bm$Province==1])
s.mean[2] <- mean(bm$averageincome[bm$Province==2])
s.mean[3] <- mean(bm$averageincome[bm$Province==3])
s.mean[4] <- mean(bm$averageincome[bm$Province==4])
s.mean[5] <- mean(bm$averageincome[bm$Province==5])
s.mean[6] <- mean(bm$averageincome[bm$Province==6])
s.mean[7] <- mean(bm$averageincome[bm$Province==7])
s.mean[8] <- mean(bm$averageincome[bm$Province==8])
s.mean[9] <- mean(bm$averageincome[bm$Province==9])


plot(s.mean)
abline(h=mean(bm$averageincome),col = "red")


mean(bm$averageincome)
mean(s.mean) # -> unequal!!!!!!!!!


pj <- vector()
pj[1] <- nrow(bm[bm$Province==1,])/nrow(bm)
pj[2] <- nrow(bm[bm$Province==2,])/nrow(bm)
pj[3] <- nrow(bm[bm$Province==3,])/nrow(bm)
pj[4] <- nrow(bm[bm$Province==4,])/nrow(bm)
pj[5] <- nrow(bm[bm$Province==5,])/nrow(bm)
pj[6] <- nrow(bm[bm$Province==6,])/nrow(bm)
pj[7] <- nrow(bm[bm$Province==7,])/nrow(bm)
pj[8] <- nrow(bm[bm$Province==8,])/nrow(bm)
pj[9] <- nrow(bm[bm$Province==9,])/nrow(bm)

sum(s.mean*pj)

## Or by a Loop

s.mean1<-vector()

for(i in 1:9){
  s.mean1[i] <- mean(bm$averageincome[bm$Province==i])
}


pj1 <- vector()
for(i in 1:9){
  pj1[i] <- nrow(bm[bm$Province==i,])/nrow(bm)
}
pj2 <-prop.table(table(bm$Province))

sum(s.mean1*pj1)


#### Boxplot

boxplot(bm$averageincome~bm$Province,
        col = c("thistle","wheat","turquoise",
                "tomato","red","yellow","blue",
                "sienna","palevioletred1"),
        names = c("Province1","Province2",
                  "Province3","Province4",
                  "Province5","Province6",
                  "Province7","Province8",
                  "Province9"),
        ylab = "avg.ic",
        main = "average income by province")

####Variance Decomposition


s.var <- vector()

for(i in 1:9){
  Nh <- length(bm$averageincome[bm$Province==i])
  s.var[i] <- var(bm$averageincome[bm$Province==i])*
    (Nh-1)/Nh
}
# -> var() is the sample variance, 
# but: s^2.w = 1/n_j*sum_{j=1}^{n_j} (x_j-mean(x_j))^2



s2.w <- sum(s.var*pj)

x.bw <- (s.mean-sum(s.mean*pj))^2*pj
s2.b <- sum(x.bw)



s2.bm <- s2.w + s2.b
s2.bm

## Alternative

v2 <- anova(lm(bm$averageincome~bm$Province))
s3 <- sum(v2[,2]/nrow(bm))

var(bm$averageincome)
## unequal!!! Variance of pop: 1/N sum_{i=1}^{N}(x_i-mu)^2

v.bm.true <- var(bm$averageincome)*(nrow(bm)-1)/nrow(bm)
v.bm.true







