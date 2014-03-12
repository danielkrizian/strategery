# In [102]: rng = pd.date_range('1/1/2012', periods=100, freq='S')
# In [103]: ts = pd.Series(np.random.randint(0, 500, len(rng)), index=rng)
# In [104]: ts.resample('5Min', how='sum')


Series.resample <- function(rule, how="last"){
# grepl(" month | months | month| months|month |months |month|months",rule, ignore.case=TRUE)
# grepl(" hour | hours | hour| hours|hour |hours |hour|hours",rule, ignore.case=TRUE)
# grepl(" min | minutes | min| minutes|min |minutes |min|minutes",rule, ignore.case=TRUE)
# grepl(" sec| secs| seconds|sec|secs|seconds",rule, ignore.case=TRUE)
  if(grepl(" quarter | quarters | quarter| quarters|quarter |quarters |quarter|quarters"
           ,rule, ignore.case=TRUE))
    if(how=="last")
      data <<- data[endpoints(Date,on = "quarters")]
  return(.self)
}

Series.pct_change <- function(periods=1){
  data[, Value:=Value / c(NA,head(Value,-periods)) - 1 , by=multivar]
  return(.self)
#   x <- x[!is.na(Return)]
}

Series <- setRefClass("Series"
                      , fields = list(data="data.table" 
                                      , multivar="character"
                      )
                      , methods = list(
#                         initialize=function(data, multivar="Instrument", index="Date")  {                       
#                           # #                           .self$initFields(...)
#                         },
                        resample=Series.resample,
                        pct_change=Series.pct_change,
                        show=function() print(data))
)

# f <- function(var.col="Ticker"){
#   ggplot(data, aes(x=Date, y=Value, colour=eval(parse(text=var.col))) + geom_line()
# }
# f()
# 
# #' always tall format
# Series.plot <- function(var.col="Instrument") {
#     
#   p <- ggplot(data, aes(x=Date, y=Value, colour=Ticker)) + geom_line() +
#     #   scale_y_continuous(trans=log10_trans())
#     # p + 
# #     facet_grid(Ticker ~ ., scales="free_y") +
#     scale_x_date(breaks=pretty_breaks(n=10), minor_breaks="year", labels=date_format("%Y")) + # scale_x_date(breaks=number_ticks(7), labels=date_format("%Y"))
# #     scale_y_continuous(labels = percent_format()) +
# #     coord_trans(y="log1p") + 
#     #
#     theme_economist_white(gray_bg=FALSE) + 
#     scale_colour_economist() +
#     xlab("") + ylab("")
#   
#   g = ggplotGrob(p)
#   panels = which(sapply(g[["heights"]], "attr", "unit") == "null")
#   g[["heights"]][panels] = list(unit(12, "cm"), unit(3, "cm"))
#   dev.off()
#   grid.draw(g)
#   
# }





