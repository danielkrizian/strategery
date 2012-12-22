##### COLOR CODES MAP #####
# http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)/
# http://research.stowers-institute.org/efg/R/Color/Chart/
# colorset = c("darkgray", "red","orange")
# redfocus, bluefocus, and greenfocus
# ? Equal-weighted: rainbow12equal, rich12equal, tim12equal, dark6equal, set8equal
# ? Monochrome: greenmono , bluemono, redmono, gray8mono and gray6mono
# bluefocusMy <- c("#0033FF", "#CCCCCC", "#D9D9D9", "#F0F0F0","#737373","#969696","#BDBDBD")
# brewer.pal(n=9,"PuBu")[c(9,6)]

library(RColorBrewer)
strategy.colors <- brewer.pal(n=9,"Blues")[c(8,6,4)]
benchmark.colors <- brewer.pal(n=9,"Greys")[c(6,5)]
random.colors <- brewer.pal(n=9,"Greys")[c(4)]

# display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)
# heat.colors(12)
# topo.colors, terrain.colors, rainbow, hsv, par

# SetTextContrastColor <- function(color)
# {
#   ifelse( mean(col2rgb(color)) > 127, "black", "white")
# }
# TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )
# 
# colCount <- 25 # number per row
# rowCount <- 27
# plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
#       axes=FALSE, ylim=c(rowCount,0))
# 
# for (j in 0:(rowCount-1))
# {
#   base <- j*colCount
#   remaining <- length(colors()) - base
#   RowSize <- ifelse(remaining < colCount, remaining, colCount)
#   rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
#        border="black",
#        col=colors()[base + (1:RowSize)])
#   text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
#        col=TextContrastColor[base + (1:RowSize)])
# }