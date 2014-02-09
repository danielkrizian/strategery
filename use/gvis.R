
data(economics)
ecm <- reshape2::melt(economics[,c('date', 'uempmed', 'psavert')], id = 'date')

p7 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineWithFocusChart')
library("rCharts", lib.loc="W:/Applications/Development/R/library")
ecm <- reshape2::melt(economics[,c('date', 'uempmed', 'psavert')], id = 'date')
p7 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineWithFocusChart')
p7$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
p7

Buy

Cl <- calc(SPX.Close)$data
Mom <- calc(mom)$data
data <- merge(Cl, Mom)
data <- data.table:::melt.data.table(data, id=1:2)

library(rCharts)
p7 <- nPlot(Value ~ Date, group = 'variable', data = data, type = 'lineWithFocusChart')
p7$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
p7

library(googleVis)
A1 <- gvisAnnotatedTimeLine(data[Date>"2010-01-01",], datevar="Date",
                            numvar="value", idvar="variable",
                            titlevar="Title", annotationvar="Annotation",
#                             options=list(displayAnnotations=FALSE,
#                                          legendPosition='newRow',
#                                          width=600, height=350)
                            options=list(displayAnnotations=TRUE,
                                         displayAnnotationsFilter=TRUE,
                                         width=900, height=350, 
                                         displayExactValues= TRUE,
                                         scaleColumns='[0,1,2]',
                                         scaleType='allmaximized',
                                         wmode='opaque')
)
plot(A1)





hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

library(googleVis)
data(Stock)
Stock
A1 <- gvisAnnotatedTimeLine(Stock, datevar="Date",
                            numvar="Value", idvar="Device",
                            titlevar="Title", annotationvar="Annotation",
                            options=list(displayAnnotations=TRUE,
                                         displayAnnotationsFilter=TRUE,
                                         legendPosition='newRow',
                                         width=600, height=350)
)
plot(A1)
