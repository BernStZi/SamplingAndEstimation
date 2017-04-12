N  <- 100
Xs <- rep(1:10,each=10)
Ys <- rep(1:10,time=10)
#Finite Population:
plot( x=Xs
     ,y=Ys
     ,pch=20
     ,axes=FALSE)
box(lwd=3)
#a sample from a finite population

sam <- sample(length(Xs),5)
points(x=Xs[sam],y=Ys[sam],pch=15,col=2,cex=2)

#Stratified population 1
plot( x=Xs
      ,y=Ys
      ,pch=20
      ,axes=FALSE)
box(lwd=3)
abline(v=seq(2,8,by=2)+0.5)
abline(h=seq(2,8,by=2)+0.5)
#or any another partion of the population
#5Splaten
schicht1 <- cbind(Xs,Ys)[c(1,2,11,12),]



#Stratified population 2
set.seed(952)
plot( x=Xs
      ,y=Ys
      ,pch=20
      ,axes=FALSE)
box(lwd=3)
rect(xleft=0  , ybottom = 0,xright = 3.5,ytop = 11,lwd=2)
text(x=2.5, y=10, labels = "1. Stratum")
rect(xleft=3.5, ybottom = 0, xright = 11, ytop = 5.5,lwd=2)#,col="goldenrod",density = 2,border = 1)
text(x=5.5, y=10, labels = "2. Stratum")
rect(xleft=3.5, ybottom = 5.5, xright = 6.5, ytop = 11,lwd=2)#,col="firebrick",density = 2,border = 1)
text(x=9.5, y=10, labels = "3. Stratum")
rect(xleft=6.5, ybottom = 5.5, xright = 11, ytop = 11,lwd=2)#,col="darkolivegreen",density = 2,border = 1)
text(x=9.5, y=5, labels = "4. Stratum")

sind <- list((1:30),c(31:35,41:45,51:55),c(61:65,71:75,81:85,91:95))
sind <- c(sind,list(setdiff(1:100,unlist(sind))))
SIND <- 1:100
SIND[sind[[1]]] <- 1
SIND[sind[[2]]] <- 2
SIND[sind[[3]]] <- 3
SIND[sind[[4]]] <- 4

strata.Xs <- split(Xs,SIND)
strata.Ys <- split(rev(Ys),SIND)
Nh <- sapply(sind,length)
nh <- round(Nh/N*n)

sam.str <- mapply(function(x,y)sample(x,y),Nh,nh)

for(i in 1:length(nh)) {
  points(x=strata.Xs[[i]][sam.str[[i]]],y=strata.Ys[[i]][sam.str[[i]]],pch=15,col=2,cex=2)
  
  
}
  

#cluster sampling
  
  
#two-stage sampling


#Variance Estimation with GREG / Calibration Estimator

