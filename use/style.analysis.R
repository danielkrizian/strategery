sourceDir(".")
library(RODBC)
library(data.table)
library(xts)
library(TTR)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(RColorBrewer)
#library(quantstrat)
options(eq.base=100)
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
last <- function(x){
  x[length(x)]
}
rm(last)

loadInstruments()
#View(instrument.table(find.instrument("Ruffer")))

src <- data.source("FalconDB", "Falcon", "falcon", "odbc", connect=T)
pfolio <- "Equity Dynamic Beta"
data.fund <- load.portfolio(find.instrument(pfolio), interval="days")
# weights
w <- c("Alken Absolute Return" =8
       , "Carmignac Patrimoine"    =10
       , "ML CCI Healthcare Long-Short"    =   10
       , "CF Ruffer Equity & General"    = 	15
       , "MW Developed Europe TOPS" 	 = 	12
       , "Pensato Europa Absolute Return" 	 = 	12
       , "Schroder GAIA Egerton" 	 = 	14
       , "Schroder GAIA Sirios" 	 = 	15)

# w <- c("Alken Absolute Return" =0
#        , "Carmignac Patrimoine"    =0
#        , "ML CCI Healthcare Long-Short"    =   100
#        , "CF Ruffer Equity & General"    =   0
#        , "MW Developed Europe TOPS" 	 = 	0
#        , "Pensato Europa Absolute Return" 	 = 	0
#        , "Schroder GAIA Egerton" 	 = 	0
#        , "Schroder GAIA Sirios" 	 = 	0)

names(w)[which(names(w)=="Schroder GAIA Egerton")] <- "Egerton European Equity"



factor.sets <- list()
factor.sets[["regions"]] <- c("NDDLNA Index", "NDDLE15 Index"
                              , "NDDLJN Index", "NDLEEGF Index")
factor.sets[["sectors"]] <- c("MSCLENR Index", "MSCLMAT Index" , "MSCLIND Index"
                              , "MSCLCDIS Index", "MSCLCSTA Index", "MSCLHC Index"
                              , "MSCLFNCL Index", "MSCLIT Index", "MSCLTEL Index"
                              , "MSCLUTI Index")
factor.sets[["styles"]] <- c("MLCLAWON Index", "MMDLAWON Index", "MSLLAWON Index"
                             , "NDLVWI Index", "NLDGWI Index")

#factors <- c(factor.sets[[3]])#, "USC0TR03 Index")
factors <- c(unlist(factor.sets), "USC0TR03 Index")
names(factors) <- NULL
rebalance <- "years"

PDT <- data.fund$prices
setkey(PDT,Name,Date)
assets <- as.character(PDT[, last(Name), by=Name]$V1)
dist <- adist(names(w), assets, fixed=F, ignore.case=T)
names(w) <- apply( dist, MARGIN=1
                   , FUN= function(r) assets[which(r== min(r))[1]])
w <- w[assets] #align/reorder with price frame
names(w) <- assets
w <- na.fill(w,fill=0)/100

reb <- PDT[,.SD[c(1,endpoints(Date, on="years")),], by=Name][,list(Name,Date)]
setkey(reb,Name,Date)
rebalance <- copy(PDT)
rebalance[,Value:=NA]
rebalance[reb,Value:=1]
rebalance <- as.xts(dcast.data.table(rebalance, Date~Name))
P <- as.xts(dcast.data.table(PDT, Date~Name))

# weights
# shares 
# positions

illiquid <- apply(P, 2
                  , function(p) ifelse(is.na(p)
                                           , na.fill(p, fill=c(FALSE, TRUE, TRUE))
                                       ,FALSE))

in.universe <- !is.na(P) | illiquid

w.portf <- t(t(in.universe) * w)
w.subset <- w.portf/rowSums(w.portf)

# Price used for portfolio holdings in valuation. 
# Can be illiquid/non-market/locf-ed price. Price is locf-ed for inside NAs only
# (no leading, no trailing)
P.val <- na.locf( P, na.rm = FALSE)
leverage <- rowSums(P.val, na.rm=T)/P.val
# if(length(rebalance)) {
   leverage[] <- ifelse(is.na(rebalance),NA,leverage)
   leverage <- na.locf(leverage, na.rm=F)
# }

shares <- w.subset * leverage
positions <- shares * P.val
P.ror <- diff.xts(P.val, arithmetic=FALSE) - 1
pnl <- rowSums(lag.xts(positions) * P.ror, na.rm=T) # daily P&L

GMV <- rowSums(abs(positions), na.rm=T) # gross market value of the portfolio

R.portf <- xts(pnl / lag.xts(GMV), order.by=index(P))
R.portf <- na.fill(R.portf,fill=0)
P.portf <- cumprod(R.portf + 1)
R.portf <- diff(to.interval(P.portf, to="months"),arithmetic=FALSE) - 1

### factors
names(factors) <- sapply(factors, function(x) getInstrument(x)$short.name)
labels <- sapply(factors, function(x) getInstrument(x)$descr)

data.factors <- returns( factors, interval="months")

R.factors <- as.xts(dcast.data.table(data.factors, Date ~ Name, fill=as.double(NA)))
names(R.factors) <- names(labels)[pmatch(names(R.factors), labels)]


R <- merge(R.portf, R.factors, join="inner")
R <- R[complete.cases(R)]

hist.returns = R["2010-01/"]

ndates = nrow(hist.returns)
n = ncol(hist.returns)-1
window.len = 36

style.weights = hist.returns[, -1]
style.weights[] = NA
style.r.squared = hist.returns[, 1]
style.r.squared[] = NA

# Setup constraints
# 0 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

library(kernlab)
# main loop
for( i in window.len:ndates ) {
  window.index = (i - window.len + 1) : i
  
  fit = lm.constraint( hist.returns[window.index, -1], hist.returns[window.index, 1], constraints )   
  style.weights[i,] = fit$coefficients
  style.r.squared[i,] = pmax(0, fit$r.squared)
}

global.fit = lm.constraint( hist.returns[, -1], hist.returns[, 1], constraints )
global.style.w <- global.fit$coefficients

betas <- round(100*global.fit$coefficients)
names(betas) <- names(style.weights)
r2 <- global.fit$r.squared

#display portfolio weights
positions <- na.fill(positions,fill=0)
weights <- positions/rowSums(positions)

library(ggplot2)
library(reshape2)
weights <- data.frame(weights["2010/"])

vars <- colnames(weights)
weights$id <- as.Date(rownames(weights))
mweights <- melt(weights, measure.vars=vars)
mweights[mweights$value==0,"value"] <- NA
tail(mweights)
ggplot(mweights, aes(id, value, colour=variable, group=variable)) + geom_line() +
  scale_x_date() + xlab("")

chart.TimeSeries(weights["2010/"], legend.loc = "topright")
colorset = 1:12
cex.legend = 0.7
pch = (1:12)
lty = 1
columnnames = colnames(weights)
element.color = "darkgray"
legend("topright", inset = 0.02, text.col = colorset, 
       col = colorset, cex = cex.legend, border.col = element.color, 
       lty = lty, lwd = 2, bg = "white", legend = columnnames, 
       pch = pch)


plot.transition.map <- function
(
  y,
  x,
  xlab = 'Risk',
  name = '',
  type=c('s','l'),
  col = NA
)
{
  if( is.list(y) ) {
    name = y$name
    x = 100 * y$risk
    y = y$weight
  }
  y[is.na(y)] = 0
  par(mar = c(4,3,2,1), cex = 0.7)
  plota.stacked(x, y, xlab = xlab, main = paste('Transition Map for', name),
                type=type[1], col=ifna(col, plota.colors(ncol(y))) )
}


aa.style.summary.plot <- function(name, style.weights, style.r.squared, window.len)
{
  layout( matrix(c(1,2,2,3,3,3), nrow=2, byrow=T) )
  #weight = t(global.style.w)
  weight = last(style.weights)
  plot.table(t(round(100*weight)))
  plota(100*style.r.squared, type='l', LeftMargin = 3, main=paste(window.len, 'months window Linear Least Squares Regression R^2'))
  plot.transition.map(style.weights, index(style.weights), xlab='', name=name)
}

aa.style.summary.plot('Style Constrained', style.weights, style.r.squared, window.len)







View(illiquid)
View(in.universe)
View(w.portf)
View(w.subset)
View(P.val)
View(leverage)
View(shares)
View(GMV)
View(pnl)
View(R.portf)


library(PerformanceAnalytics)
R <- R["2010/"]
chart.RollingStyle(R[,1,drop=FALSE], R[,-1],
                   method="constrained", leverage=FALSE
                   , width=26, cex.legend = 0.5, col=brewer.pal(9,"PuRd"))

tbl <- table.RollingStyle(R[,1,drop=FALSE], R[,-1],
                    method="constrained", leverage=TRUE, width=26)


xts:::rollapply.xts(R
                    , FUN= function(x, method, leverage) {
                      t(style.fit(R.fund = x[,1,drop=FALSE]
                                  , R.style = x[,-1,drop=FALSE]
                                  , method = method
                                  , leverage = leverage)$R.squared)}
                    , width = 24
                    , method = "constrained"
                    , leverage = FALSE
                    , by = 1
                    , by.column = FALSE
                    , na.pad = FALSE
                    , align = "right")


# SIT ---------------------------------------------------------------------


symbols = spl('FWWFX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY')      
symbol.names = spl('Fund,Australia,Canada,France,Germany,Japan,UK,USA') 
getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)

# align dates for all symbols & convert to frequency 
hist.prices = merge(FWWFX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY)      
period.ends = endpoints(hist.prices, 'months')
hist.prices = Ad(hist.prices)[period.ends, ]

index(hist.prices) = as.Date(paste('1/', format(index(hist.prices), '%m/%Y'), sep=''), '%d/%m/%Y')
colnames(hist.prices) = symbol.names

# remove any missing data   
hist.prices = na.omit(hist.prices['1990::2010'])

# compute simple returns    
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )

#load 3-Month Treasury Bill from FRED
TB3M = quantmod::getSymbols('TB3MS', src='FRED', auto.assign = FALSE)   
TB3M = processTBill(TB3M, timetomaturity = 1/4)
index(TB3M) = as.Date(paste('1/', format(index(TB3M), '%m/%Y'), sep=''), '%d/%m/%Y')
TB3M = ROC(Ad(TB3M), type = 'discrete')
colnames(TB3M) = 'Cash'

# add Cash to the asset classes
hist.returns = na.omit( merge(hist.returns, TB3M) )



