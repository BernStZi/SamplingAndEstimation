# Functions

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
