library(lattice)
bbar <- seq(5, 25, by = .5)
rho  <- seq(.02, .2, by=.005)
deff <- function(bbar,rho) 1+ ((bbar-1)*rho)
D <- sapply(bbar, function(x) sapply(rho, function(y) deff(x,y)))
dimnames(D) <- list(rho, bbar)

a.pts <- data.frame(x = c(15,15,15),
                    y = c(.05,.10,.15),
                    z = NA)
a.pts$z <- 1 + ((a.pts$x-1)*a.pts$y)
a <- sort(c(1:6,a.pts$z))

b.pts <- data.frame(x = c(20,20,20),
                    y = c(.05,.10,.15),
                    z = NA)
b.pts$z <- 1 + ((b.pts$x-1)*b.pts$y)
b <- sort(c(1:6,b.pts$z))

f <- unique(sort(c(a,b)))
cols <-  rep("black",length(f))
cols[f%in%a.pts$z] <- "red"
cols[f%in%b.pts$z] <- "green"
cols <- cols[-1]

deffTildeBbarRho <- 
wireframe(D ~ rep(bbar,each=length(rho)) + rep(rho,length(bbar)),
          drape = TRUE,
          at = seq(1,6,by=.5),
          colorkey = TRUE,
          col.regions = c("blue2","grey70"),
          alpha.regions = .5,
          xlab = expression(bar(b)),
          ylab = expression(rho),
          zlab = expression(Deff[c]),
          default.scales = list(arrows = FALSE,
                                x = list(at = c(seq(5,25,by=5),max(bbar))),
                                y = list(at = c(seq(.05,max(rho),by=.05),max(rho))),
                                z = list(at = f, col = cols)),
          a.pts = a.pts,
          b.pts = b.pts,
          screen =list(x=-50,y=55,z=35),
          panel.3d.wireframe = function(x,y,z,
                                        xlim, ylim, zlim,
                                        xlim.scaled, ylim.scaled, zlim.scaled,
                                        a.pts, b.pts, ...){

                                a.xx <- ((a.pts$x -min(bbar))/ (max(bbar)-min(bbar)))-.5
                                a.yy <- ((a.pts$y -min(rho))/ (max(rho)-min(rho)))-.5
                                a.zz <- ((a.pts$z -min(D))/ (max(D)-min(D)))-.5

                                b.xx <- ((b.pts$x -min(bbar))/ (max(bbar)-min(bbar)))-.5
                                b.yy <- ((b.pts$y -min(rho))/ (max(rho)-min(rho)))-.5
                                b.zz <- ((b.pts$z -min(D))/ (max(D)-min(D)))-.5

                                panel.3dscatter(x = a.xx, y = a.yy, z = a.zz,
                                                xlim = xlim,
                                                ylim = ylim,
                                                zlim = zlim,
                                                xlim.scaled = xlim.scaled,
                                                ylim.scaled = ylim.scaled,
                                                zlim.scaled = zlim.scaled,
                                                type = "h", pch = 0, lwd = 5, col = "red",...)

                                panel.3dscatter(x = b.xx, y = b.yy, z = b.zz,
                                                xlim = xlim,
                                                ylim = ylim,
                                                zlim = zlim,
                                                xlim.scaled = xlim.scaled,
                                                ylim.scaled = ylim.scaled,
                                                zlim.scaled = zlim.scaled,
                                                type = "h", pch = 0, lwd = 5, col = "green",...)

                                panel.3dwire(x,y,z,
                                             xlim = xlim,
                                             ylim = ylim,
                                             zlim = zlim,
                                             xlim.scaled = xlim.scaled,
                                             ylim.scaled = ylim.scaled,
                                             zlim.scaled = zlim.scaled,
                                             lwd = 0.25,...)

                                for(i in 1:nrow(a.pts)){
                                  panel.3dscatter(x = c(a.xx[i],-.5,-.5),
                                                  y = c(a.yy[i],a.yy[i],.5),
                                                  z = c(a.zz[i],a.zz[i],a.zz[i]),
                                  xlim = xlim,
                                  ylim = ylim,
                                  zlim = zlim,
                                  xlim.scaled = xlim.scaled,
                                  ylim.scaled = ylim.scaled,
                                  zlim.scaled = zlim.scaled,
                                  pch = 2, col = "red", lwd = 4, type="l",...)}

                                for(i in 1:nrow(b.pts)){
                                  panel.3dscatter(x = c(b.xx[i],-.5,-.5),
                                                  y = c(b.yy[i],b.yy[i],.5),
                                                  z = c(b.zz[i],b.zz[i],b.zz[i]),
                                  xlim = xlim,
                                  ylim = ylim,
                                  zlim = zlim,
                                  xlim.scaled = xlim.scaled,
                                  ylim.scaled = ylim.scaled,
                                  zlim.scaled = zlim.scaled,
                                  pch = 2, col = "green", lwd = 4, type="l",...)}
                               })
