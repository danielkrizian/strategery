require(PerformanceAnalytics)

require(RGraphics)
require(gridBase)
require(gridExtra)
require(xtsExtra)
require(Hmisc)
require(lattice)

#' Some title
#'@export
do_layout <- function (x, screens, layout.screens, yax.loc, nc, nr, ylim) 
{
  #customized par(oma
  screens <- factor(if (identical(screens, "auto")) 
    seq_len(NCOL(x))
  else rep(screens, length.out = NCOL(x)))
  if (identical(layout.screens, "auto")) {
    layout.screens <- seq_along(levels(screens))
    if (!missing(nc) && !missing(nr)) 
      layout.screens <- matrix(layout.screens, ncol = nc, 
        nrow = nrow)
    if (missing(nc) && !missing(nr)) 
      layout.screens <- matrix(layout.screens, nrow = nr)
    if (!missing(nc) && missing(nr)) 
      layout.screens <- matrix(layout.screens, ncol = nc)
  }
  if (is.list(layout.screens)) {
    layout.args <- layout.screens[-1L]
    layout.screens <- layout.screens[[1L]]
  }
  layout.screens <- as.matrix(layout.screens)
  have_x_axis <- logical(length(levels(screens)))
  for (i in seq_len(NROW(layout.screens))) {
    if (i == NROW(layout.screens)) {
      have_x_axis[layout.screens[i, ]] <- TRUE
    }
    else {
      if (!identical(as.logical(diff(layout.screens[i, 
        ])), as.logical(diff(layout.screens[i + 1L, 
        ])))) {
        have_x_axis[layout.screens[i, ]] <- TRUE
      }
    }
  }
  have_y_axis <- logical(length(levels(screens)))
  for (i in seq_len(NCOL(layout.screens))) {
    if (i == 1L) {
      have_y_axis[layout.screens[, i]] <- TRUE
    }
    else {
      if (!identical(as.logical(diff(layout.screens[, 
        i - 1L])), as.logical(diff(layout.screens[, 
        i])))) {
        have_y_axis[layout.screens[, i]] <- TRUE
      }
    }
  }
  ylab.axis <- layout.screens
  if (yax.loc == "none") 
    ylab.axis[] <- "none"
  if (yax.loc == "right" || yax.loc == "left") {
    have_y_axis[] <- TRUE
    ylab.axis[] <- yax.loc
  }
  if (yax.loc == "out" || yax.loc == "in") {
    if (NCOL(layout.screens) != 2L) 
      stop("yax.loc not consistent with layout -- too many columns.")
    ylab.axis[, 1L] <- if (yax.loc == "out") 
      "left"
    else "right"
    ylab.axis[, 2L] <- if (yax.loc == "out") 
      "right"
    else "left"
    have_y_axis[] <- TRUE
  }
  if (yax.loc == "flip") {
    for (i in seq_len(NCOL(ylab.axis))) ylab.axis[, i] <- rep(c("left", 
      "right"), length.out = NROW(ylab.axis))
    have_y_axis[] <- TRUE
  }
  if (yax.loc == "top") {
    ylab.axis[] <- yax.loc
    have_y_axis[] <- TRUE
    have_x_axis[] <- TRUE
  }
  if (length(levels(screens)) > 1L) 
#     par(oma = c(1, 1, 4, 1))
    par(oma = c(1, 1, 1, 1)-1)
  if (yax.loc == "none") 
#     par(oma = c(1, 4, 4, 3))
    par(oma = c(1, 1, 1, 1)-1)
  if (length(levels(screens)) == 1L && yax.loc != "none") 
#     par(oma = c(1, 1, 4, 1))
      par(oma = c(1, 1, 1, 1)-1)
  if (identical(ylim, "fixed")) {
    ylim <- list(range(x, na.rm = TRUE))
  }
  else if (identical(ylim, "auto")) {
    if (yax.loc == "none") {
      ylim <- lapply((1:NROW(layout.screens))[!duplicated(layout.screens)], 
        function(y) {
          do.call(range, list(xtsExtra:::split.xts.by.cols(x, screens)[layout.screens[y, 
            ]], na.rm = TRUE))
        })
    }
    else {
      ylim <- lapply(xtsExtra:::split.xts.by.cols(x, screens), function(x) range(x, 
        na.rm = TRUE))
    }
  }
  else {
    if (!is.matrix(ylim)) 
      dim(ylim) <- c(1L, NROW(ylim))
    ylim <- lapply(1:NROW(ylim), function(x) ylim[x, 1:2])
  }
  if (length(layout.screens) > 1L) {
    if (!exists("layout.args")) {
      layout(layout.screens, heights = 1 + 0.05 * NROW(unique(layout.screens)) * 
        apply(layout.screens, 1L, function(j) any(have_x_axis[j])))
    }
    else {
      do.call(layout, c(list(layout.screens), layout.args))
    }
  }
  return(list(layout.screens = layout.screens, screens = screens, 
    have_x_axis = have_x_axis, have_y_axis = have_y_axis, 
    ylab.axis = ylab.axis, ylim = ylim))
}

#' Some title
#'@export
plot.xts.custom <- function (x, y = NULL, screens = "auto", layout.screens = "auto", 
  ..., yax.loc = c("none", "out", "in", "flip", "left", "right", 
    "top"), auto.grid = TRUE, major.ticks = "auto", minor.ticks = TRUE, 
  major.format = TRUE, bar.col.up = "white", bar.col.dn = "red", 
  candle.col = "black", xy.labels = FALSE, xy.lines = NULL, 
  ylim = "auto", panel = default.panel, auto.legend = FALSE, 
  legend.names = colnames(x), legend.loc = "topleft", legend.pars = NULL, 
  events, blocks, nc, nr) 
{
  if (length(screens) > 1L || (NCOL(x) > 1L && identical(screens, 
    "auto"))) 
    par(cex.lab = 0.8)
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  on.exit(assign(".plot.xts", recordPlot(), .GlobalEnv), add = TRUE)
  dots <- list(...)
  do.call(par, dots[!(names(dots) %in% c("col", "type", "lwd", 
    "pch", "log", "cex", "ylab", "main", "axes", "xlab", 
    "lty"))])
  if (!is.null(y)) {
    xlab <- if ("xlab" %in% names(dots)) 
      dots[["xlab"]]
    else deparse(substitute(x))
    ylab <- if ("ylab" %in% names(dots)) 
      dots[["ylab"]]
    else deparse(substitute(y))
    if (NCOL(x) > 1L || NCOL(y) > 1L) {
      layout(matrix(seq_len(NCOL(x) * NCOL(y)), ncol = NCOL(x), 
        nrow = NCOL(y)))
#       par(mar = c(2, 2, 2, 2), oma = c(0, 0, 3, 0))
      browser()
            par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
      for (i in seq_len(NCOL(x))) {
        for (j in seq_len(NCOL(y))) {
          do_scatterplot(x[, i], y[, j], xy.labels, 
            xy.lines, xlab = "", ylab = "", ..., main = paste(names(x)[i], 
              "vs.", names(y)[j]))
        }
      }
      mtext(paste(xlab, "vs.", ylab), outer = TRUE, line = 0)
      return(invisible(merge(x, y)))
    }
    else {
      return(do_scatterplot(x, y, xy.labels, xy.lines, 
        xlab, ylab, ...))
    }
  }
  main <- if (!("main" %in% names(dots))) 
    deparse(substitute(x))
  else dots[["main"]]
  x <- try.xts(x)
  if ("xlim" %in% names(dots)) {
    xlim <- dots[["xlim"]]
    if (is.timeBased(xlim)) {
      if (length(xlim) != 2L) 
        stop("Need endpoints only for xlim")
      xlim <- do.call(paste0("as.", indexClass(x))[1L], 
        list(xlim))
      x <- x[(index(x) > xlim[1L]) & (index(x) < xlim[2L]), 
        , drop = FALSE]
    }
    if (is.numeric(xlim)) {
      warning("Using xlim as row indices -- provide timeBased xlim", 
        "if you wish to subset that way")
      x <- x[xlim[1L]:xlim[2L], drop = FALSE]
    }
    if (is.character(xlim)) {
      x <- x[xlim, , drop = FALSE]
    }
  }
  yax.loc <- match.arg(yax.loc)
  if ("type" %in% names(dots) && dots[["type"]] %in% c("candles", 
    "bars")) {
    type <- dots[["type"]]
    if (!xts:::is.OHLC(x)) 
      stop(type, "-chart not supported for non-OHLC series")
    do_plot.ohlc(x, bar.col.up = bar.col.up, bar.col.dn = bar.col.dn, 
      candle.col = candle.col, major.ticks = major.ticks, 
      minor.ticks = minor.ticks, auto.grid = auto.grid, 
      major.format = major.format, main = main, candles = (type == 
        "candles"), events = events, blocks = blocks, 
      yax.loc = yax.loc, ylim = ylim, ...)
  }
  else {
    screens <- do_layout(x, screens = screens, layout.screens = layout.screens, 
      yax.loc = yax.loc, nc = nc, nr = nr, ylim = ylim)
    layout.screens <- screens[["layout.screens"]]
    have_x_axis <- screens[["have_x_axis"]]
    have_y_axis <- screens[["have_y_axis"]]
    ylab.axis <- screens[["ylab.axis"]]
    ylim <- screens[["ylim"]]
    screens <- screens[["screens"]]
    x.split <- xtsExtra:::split.xts.by.cols(x, screens)
    ylab <- dots[["ylab"]]
    if (is.null(ylab)) {
      if (is.null(names(x))) 
        ylab <- ""
      else ylab <- split(names(x), screens)
    }
    log <- dots[["log"]]
    if (is.null(log)) 
      log <- ""
    if (auto.legend) 
      legend.names <- split(legend.names, screens)
    for (i in seq_along(levels((screens)))) {
      x.plot <- x.split[[i]]
      col.panel <- xtsExtra:::get.elm.from.dots("col", dots, screens, 
        i)
      pch.panel <- xtsExtra:::get.elm.from.dots("pch", dots, screens, 
        i)
      cex.panel <- xtsExtra:::get.elm.from.dots("cex", dots, screens, 
        i)
      lwd.panel <- xtsExtra:::get.elm.from.dots("lwd", dots, screens, 
        i)
      type.panel <- xtsExtra:::get.elm.from.dots("type", dots, screens, 
        i)
      lty.panel <- xtsExtra:::get.elm.from.dots("lty", dots, screens, 
        i)
      log.panel <- xtsExtra:::get.elm.recycle(log, i)[[1L]]
      ylab.panel <- xtsExtra:::get.elm.recycle(ylab, i)[[1L]]
      panel.panel <- match.fun(if (length(panel) > 1L) 
        xtsExtra:::get.elm.recycle(panel, i)
      else panel)
      xtsExtra:::do_add.grid(x.plot, major.ticks, major.format, minor.ticks, 
        auto.grid = auto.grid, ylab = ylab.panel, log = log.panel, 
        have_x_axis = have_x_axis[i], have_y_axis = have_y_axis[i], 
        ylab.axis = ylab.axis[which.max(layout.screens == 
          i)], events = events, blocks = blocks, yax.loc = yax.loc, 
        ylim = xtsExtra:::get.elm.recycle(ylim, i))
      legend.pars.add <- xtsExtra:::do_add.panel(x.plot, panel = panel.panel, 
        col = col.panel, lwd = lwd.panel, pch = pch.panel, 
        type = type.panel, cex = cex.panel, lty = lty.panel)
      if (auto.legend && !is.na(xtsExtra:::get.elm.recycle(legend.loc, 
        i))) 
        do.call(xtsExtra:::do_add.legend, c(legend.names = list(legend.names[[i]]), 
          legend.loc = xtsExtra:::get.elm.recycle(legend.loc, i), 
          legend.pars.add, legend.pars))
    }
  }
  title(main, outer = TRUE)
  return(invisible(reclass(x)))
}

#' Some title
#'@export
default.panel.rev <- function (index, x, col, pch, cex, lwd, type, lty) 
{
# modelled after xtsExtra::default.panel except the order of lines is reversed for better visibility: seq_len(NCOL(x)) is substituted with NCOL(x):1
    for (j in NCOL(x):1) {
        col.t <- xtsExtra:::get.elm.recycle(col, j)
        pch.t <- xtsExtra:::get.elm.recycle(pch, j)
        cex.t <- xtsExtra:::get.elm.recycle(cex, j)
        lwd.t <- xtsExtra:::get.elm.recycle(lwd, j)
        type.t <- xtsExtra:::get.elm.recycle(type, j)
        lty.t <- xtsExtra:::get.elm.recycle(lty, j)
        lines(index, x[, j], col = col.t, pch = pch.t, type = type.t, 
            lwd = lwd.t, cex = cex.t, lty = lty.t)
    }
}

#' Some title
#'@export
cumreturn.panel  <- function(index,x, ...) {
  title(ylab="Cumulative Performance (Log)")
  default.panel.rev(index,x,...)
  par(yaxt="s") # enable axis drawing
  axis(2, at=axTicks(2), labels=as.dollar(axTicks(2)), hadj=0.7)
  par(yaxt="n") # disable axis drawing
}

#' Some title
#'@export
drawdown.panel <-  function(index,x,...) {  
#   par(new=T)
  title(ylab="Drawdown", ps=10)
  default.panel.rev(index, x, ...)
  par(yaxt="s") # enable axis drawing
  axis(2, at=axTicks(2), labels=as.percent(x=axTicks(2)), hadj=0.7)
  par(yaxt="n") # disable axis drawing
}

#' Some title
#'@export
Plot.Equity <- function
(... # objects to draw. Currently supported portfolios and return xts
 ,nas=NULL # logical, does R contain NAs? If NULL, function will detect, but costly speedwise
 ,sbo=NULL # vector of integers, identifying columns in provided objects as either 
#  strategy, benchmark or other (SBO). Strategy denoted as 1, benchmark as 2, other as 0
 ,init=100 # numeric, value of the starting index
 ,lead.na.rm=F # Should leading NAs be removed?
 ,dates=c("first","longest") #if "first", R will be subset by indices of first series
 ,short.align=c("first", "longest", "none") # align start of the shorter indices to which series?
 ,ddpanel=F # display drawdown panel?
 ,names=NULL
) {
#   TODO: lead.na.rm=T not tested

 dots <- list(...)
 R <- NULL
 for(o in dots) {
   if(any(class(o)=="portfolio"))
     o <- Returns(o, reduce=F)
   R <- if(is.null(R)) o else merge(R,o, all=T)
 }
 
  if(length(names))
    colnames(R) <- names[1:NCOL(R)]

#### compute index ####
 
  if(!is.null(nas))
    if(!nas)
      index <- cumprod(1+R) # shortcut for clean version of R
 
    #### helper functions
    .equity <- function(x) cumprod(1+x)
    .peak <- function(x) cummax(x)
    applyNAsafe <- function(x, .fun, ...){
      cleanx <- na.omit(x)
      if(length(attr(cleanx,"na.action"))){
        out <- x
        out[attr(cleanx,"na.action")] <- NA
        out[-attr(cleanx,"na.action")] <- do.call(.fun, list(cleanx, ...) )
      } else
        out <- do.call(.fun, list(cleanx, ...) )
      return(out)
    }
    
    #### preprocess data
    # has leading NAs or just middle NAs
    # middle NAs - fill them with zeroes; leading NAs - leave them in place
    dates <- dates[1]
    if (dates == "first") {
      first.dates <- string.range(na.omit(R[,1]))
      R <- R[first.dates]
    }
    
    .data <- coredata(R)
    lead.na <- if(lead.na.rm) NULL else NA
    .data <- apply(.data, 2 , na.fill, fill=list(lead.na,0,0))
    
    #### calculate equity index
    index <- apply(.data,2, applyNAsafe , .fun=.equity)
    
    #### align shorter indices
    short.align <- short.align[1]
    if(short.align=="none")
      init <- rep(init,NCOL(R))
    else if(short.align=="longest"){
      i.lastna <- colSums(is.na(index))
      longest <- which.min(i.lastna)
      init <- sapply(i.lastna, function(x) if(x>0) init * index[x, longest] else init)
    } else if(short.align=="first") {
      i.lastna <- colSums(is.na(index))
      init <- sapply(i.lastna, function(x) if(x>0) init * index[x, 1] else init)
    }
    index <- t(t(index) * init) # multiply matrix by vector
    index <- reclass(index,R)

 
#### calculate drawdown ####
  if(ddpanel){
    peak <- apply(index, 2, applyNAsafe , .fun=.peak)
    drawdown <- index/peak - 1
    drawdown=reclass(drawdown,R)
  }
  
#### plot ####
 panels <- cumreturn.panel
 toplot <- index
 screens <- rep(1,NCOL(index))
  if(ddpanel) {
    panels <- c(panels,drawdown.panel)
    toplot <- merge(index,drawdown)
    colnames(toplot) <- c(colnames(index),colnames(drawdown))
    screens <- c(screens,rep(2,NCOL(drawdown)))
  }

 if(is.null(sbo))
   sbo <- c(1:min(2,NCOL(index)),rep(0,NCOL(index)-min(2,NCOL(index))))
 if(length(sbo)<NCOL(index))
   sbo <- c(sbo, rep(0,NCOL(index)-length(sbo)))
 
  plot.xts.custom( toplot
           ,screens = screens
           ,layout.screens = c(1,1,ifelse(ddpanel,2,1))
           ,panel = panels
           # series
           ,lwd=c(rep(3,sum(sbo==1)),
                  rep(1,sum(sbo==2)),
                  rep(0.5,sum(sbo==0))) #line boldness
           ,col=c(strategy.colors[unique(cumsum(sbo==1))],
                  benchmark.colors[unique(cumsum(sbo==2))],
                  rep(random.colors,sum(sbo==0)))  #line colors
           ,auto.grid=F
           # both axes
           ,las = 1 # axis labels always horizontal
           ,tcl=-0.2 # tick marks size
           ,cex.lab=1 # axis labels text size
#            ,cex.axis=0.7 # axis labels text size
           ,ps=10 # font size
           # x-axis
           ,minor.ticks = F # remove x-axis minor ticks
           ,major.format = "%Y"
           ,xaxs="i" # no white space between axis and series
           # y-axis
           ,ylab=NA # suppress y axis labels to control via panel function
           ,yaxt="n" # #suppress axis ticks for customization in panel function
           ,yax.loc="left"
#            ,yaxs="i" # no white space between axis and series 
           ,log= c("y","") # cumreturns panel has log scale
           # legend
           ,legend.loc = c("topleft",NA),
           ,legend.pars=list(bty="n", horiz=F, ncol=2) # legend box transparent, vertical labels
           ,auto.legend=T
           # plot
           ,bty = "l" # box around plot - "L" shape
           ,main=NA
  )
}

#' Some title
#'@export
Plot.returns <- function(R, scale=c("monthly", "yearly"), last=10) {
  
  require(reshape)
  if(length(scale)>1) scale <- scale[1]
  if(scale=="monthly") {
    mret <-compute.monthly.returns(R=R)
    tbl <- table.CalendarReturns(mret, digits=8, as.perc=FALSE)
    tbl <- tail(tbl,10)
    year.name <- rownames(tbl)
    colnames(tbl) <- c(month.abb, "Ann.")
    tbl <- round(tbl*100,1)
    tbl$Year <- rownames(tbl)
    mtbl <-melt(tbl)
    ttbl <- transform(mtbl, Year=as.factor(Year), Month=as.factor(variable))
    
    cols <- c('darkred','red','white','lightgreen' ,'darkgreen') 
    brks <- c(-5,-2, 0, 2, 20)
    p <- ggplot(ttbl)
    p <- p + geom_tile(aes_string( x = "Month", y = "Year", fill = "value"))
    p <- p +  scale_fill_gradientn(name = 'Return (%)'
                                   ,limits = c(-5, 20)
                                   ,colours = cols, breaks=brks
                                   ,labels=brks, values= brks
                                   ,rescaler = function(x, ...) x, oob = identity)
    p <- p + geom_text(aes(x = Month, y = Year, label=value)
                       ,colour="black"
                       ,size=3)
    return(p)
  } else {
    
  }
}



chart.TimeSeries <- function (R, auto.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, 
                              type = "l", lty = 1, lwd = 2, main = NULL, ylab = NULL, 
                              xlab = "Date", date.format.in = "%Y-%m-%d", date.format = NULL, 
                              xlim = NULL, ylim = NULL, element.color = "darkgray", event.lines = NULL, 
                              event.labels = NULL, period.areas = NULL, event.color = "darkgray", 
                              period.color = "aliceblue", colorset = (1:12), pch = (1:12), 
                              legend.loc = NULL, ylog = FALSE, cex.axis = 0.8, cex.legend = 0.8, 
                              cex.lab = 1, cex.labels = 0.8, cex.main = 1, major.ticks = "auto", 
                              minor.ticks = TRUE, grid.color = "lightgray", grid.lty = "dotted", 
                              xaxis.labels = NULL, padj=-1, bty="o", inset = 0.02, tcl=-0.1, ...) 
{
  # strange bug base:::t
  # main = NULL, ylab=NULL enabled
  # padj=-1 argument added - adjusts axis labels perpendicular to the reading direction
  # bty="o" argument added - c("o","n") whether legend background should be transparent. Also exposed in legend() below
  # inset=0.02 argument added - whether legend should be moved towards plot margins. Also exposed in legend() below instead of constant exposure=0.02
  # tcl=-0.1 The length of tick marks as a fraction of the height of a line of text.
  
  y = checkData(R)
  columns = ncol(y)
  rows = nrow(y)
  columnnames = colnames(y)
  if (is.null(date.format)) {
    freq = periodicity(y)
    yr_eq <- ifelse(format(index(first(y)), format = "%Y") == 
      format(index(last(y)), format = "%Y"), TRUE, FALSE)
    switch(freq$scale, seconds = {
      date.format = "%H:%M"
    }, minute = {
      date.format = "%H:%M"
    }, hourly = {
      date.format = "%d %H"
    }, daily = {
      if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"
    }, weekly = {
      if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"
    }, monthly = {
      if (yr_eq) date.format = "%b" else date.format = "%b %y"
    }, quarterly = {
      if (yr_eq) date.format = "%b" else date.format = "%b %y"
    }, yearly = {
      date.format = "%Y"
    })
  }
  rownames = as.Date(time(y))
  rownames = format(strptime(rownames, format = date.format.in), 
                    date.format)
  time.scale = periodicity(y)$scale
  ep = axTicksByTime(y, major.ticks, format.labels = date.format)
  logaxis = ""
  if (ylog) {
    logaxis = "y"
  }
  if (is.null(xlim[1])) 
    xlim = c(1, rows)
  if (is.null(ylim[1])) {
    ylim = as.numeric(range(y, na.rm = TRUE))
  }
  plot.window(xlim, ylim, xaxs = "r", log = logaxis)
  # custom code
  #   if (is.null(ylab)) {
  #     if (ylog) 
  #       ylab = "ln(Value)"
  #     else ylab = "Value"
  #   }
  # end custom code
  if (ylog) 
    dimensions = 10^par("usr")
  else dimensions = par("usr")
  if (!is.null(period.areas)) {
    period.ind = NULL
    for (period in 1:length(period.areas)) {
      period.ind = list(grep(period.areas[[period]][1], 
                             rownames), grep(period.areas[[period]][2], rownames))
      rect(period.ind[1], dimensions[3], period.ind[2], 
           dimensions[4], col = period.color, border = NA)
    }
  }
  if (auto.grid) {
    abline(v = ep, col = grid.color, lty = grid.lty)
    grid(NA, NULL, col = grid.color)
  }
  abline(h = 0, col = element.color)
  if (!is.null(event.lines)) {
    event.ind = NULL
    for (event in 1:length(event.lines)) {
      event.ind = c(event.ind, grep(event.lines[event], 
                                    rownames))
    }
    number.event.labels = ((length(event.labels) - length(event.ind) + 
      1):length(event.labels))
    abline(v = event.ind, col = event.color, lty = 2)
    if (!is.null(event.labels)) {
      text(x = event.ind, y = ylim[2], label = event.labels[number.event.labels], 
           offset = 0.2, pos = 2, cex = cex.labels, srt = 90, 
           col = event.color)
    }
  }
  if (length(lwd) < columns) 
    lwd = rep(lwd, columns)
  if (length(lty) < columns) 
    lty = rep(lty, columns)
  if (length(pch) < columns) 
    pch = rep(pch, columns)
  for (column in columns:1) {
    lines(1:rows, y[, column], col = colorset[column], lwd = lwd[column], 
          pch = pch[column], lty = lty[column], type = type, 
          ...)
  }
  if (xaxis) {
    if (minor.ticks) 
      axis(1, at = 1:NROW(y), col=element.color, labels = FALSE, col = "#BBBBBB")
    # custom code
    #     label.height = cex.axis * (0.5 + apply(t(names(ep)), 1, function(X) max(strheight(X, units = "in")/par("cin")[2])))
    label.height = cex.axis * (0.5 + apply(base:::t(names(ep)), 1, function(X) max(strheight(X, units = "in")/par("cin")[2])))
    # end custom code
    if (is.null(xaxis.labels)) 
      xaxis.labels = names(ep)
    else ep = 1:length(xaxis.labels)
    axis(1, at = ep, labels = xaxis.labels, las = 1, lwd = 1, 
         mgp = c(3, label.height, 0), cex.axis = cex.axis, col=element.color, padj=padj, tcl=tcl)
    title(xlab = xlab, cex.lab = cex.lab)
  }
  if (yaxis) 
    if (yaxis.right) 
      axis(4, cex.axis = cex.axis, col = element.color, 
           ylog = ylog)
  else axis(2, cex.axis = cex.axis, col = element.color, 
            ylog = ylog)
  box(col = element.color)
  if (!is.null(legend.loc)) {
    legend(legend.loc, inset = inset, text.col = colorset, 
           col = colorset, cex = cex.legend, border.col = element.color, 
           lty = lty, lwd = 2, bty=bty, bg = "white", legend = columnnames)
  }
  # custom code
  #   if (is.null(main)) 
  #     main = columnnames[1]
  if (!is.null(main)) title(main = main, cex.main = cex.main)
  if (!is.null(ylab)) title(ylab = ylab, cex.lab = cex.lab)
  #   title(ylab = ylab, cex = cex.lab)
  # end custom code
  
  
}


chart.SideBar <- function (w,  auto.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, 
                           type = "l", lty = 1, lwd = 2, main = NULL, ylab = "Annualized Returns", xlab = NULL, 
                           xlim = NULL, ylim = NULL, element.color = "darkgray", event.lines = NULL, 
                           event.labels = NULL, period.areas = NULL, event.color = "darkgray", 
                           period.color = "aliceblue", colorset = (1:12), pch = (1:12), 
                           legend.loc = NULL, cex.axis = 0.8, cex.legend = 0.8, 
                           cex.lab = 1, cex.labels = 0.8, cex.main = 1, major.ticks = "auto", 
                           minor.ticks = TRUE, grid.color = "lightgray", grid.lty = "dotted", 
                           xaxis.labels = NULL, ...) 
{
  #  source: http://www.r-bloggers.com/reporting-good-enough-to-share/
  #this function shows bar plot side-by-side comparisons for rolling annualized returns
#   not adapted yet to my purposes
  barplot(w, beside=TRUE, col = colorset[1:NROW(w)], 
          xlab = xlab, ylab = ylab, axes = FALSE,
          ylim = c(min(0,min(w)),max(w)+0.05),...)
  
  if (auto.grid) {
    abline(v=0, col = element.color, lty = grid.lty)
    grid(NA, NULL, col = grid.color)
  }
  abline(h = 0, col = grid.color)
  
  axis(2, cex.axis = cex.axis, col = element.color)
  title(ylab = ylab, cex = cex.lab)
  
  
  
  if (!is.null(legend.loc)) {
    legend(legend.loc, inset = 0.02, text.col = colorset, 
           col = colorset, cex = cex.legend, border.col = grid.color, 
           lty = lty, lwd = 2, bg = "white", legend = rownames(w))
  }
  
  box(col = element.color)
}



# 
# chart.Bar <- function (R, legend.loc = NULL, colorset = (1:12), as.perc=TRUE, ...) 
# {
#     x = checkData(R)
#     chart.TimeSeries(x, type = "h", colorset = colorset, legend.loc = legend.loc, 
#         lend = "butt", as.perc=as.perc, ...)
# }