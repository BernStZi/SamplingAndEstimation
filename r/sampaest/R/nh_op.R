nh_op <- function(Nh.tab,opt,strind,n){
  tab.names     <- apply(expand.grid(dimnames(Nh.tab)),1,paste,collapse="_")
  V.h   <- tapply(opt,strind,sd)[tab.names]
  nh.op <- round((Nh.tab*V.h)/(sum(Nh.tab*V.h))*n)
  return(nh.op)
}
