data(airquality)
# Define chart
pairs.chrt <- ggpairs(airquality,
                      lower = list(continuous = "smooth"),
                      diag = list(continuous = "blank"),
                      upper = list(continuous = "blank")) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(angle = 180, vjust = 1, color = "black"),
        panel.border = element_rect(fill = NA),
        axis.text.x =element_text(size=50) ) 

# Device off and print
print(pairs.chrt)

# Plot in current window
# use left to add space at y axis and bottom for below xaxis
# see ?print.ggpairs
print(pairs.chrt, left = 1, bottom = 1)

# Get list of grobs in current window and extract the axis labels
# note if you add a title this will add another text grob, 
# so you will need to tweak this so not to extract it
g <- grid.ls(print=FALSE)
idx <- g$name[grep("text", g$name)]

# Rotate yaxis labels
# change the rot value to the angle you want
for(i in idx[1:6]) {
  grid.edit(gPath(i), rot=0, hjust=0.25, gp = gpar(col="red"))
}

# Remove extra ones if you want
n <- ncol(airquality)
lapply(idx[c(1, 2*n)], grid.remove)
