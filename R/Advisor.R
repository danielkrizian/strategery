Advisor <- setRefClass("Advisor",
                       fields=list(
                         rules="list" # ?
                       ),
                       methods=list(
                         
                         extract.signal = function(rule) {
                           s = rule$signal
                           if(is.sfl(s))
                             s = eval.sfl(s)
                           s$data[eval(as.name(s$.col)) == TRUE, c(s$.id, s$.time), with=FALSE]
                         },
                         
                         model.portfolio = function(signal, value) {
                             signal[, Units:=value]
                         },
                         
                         signals = function(){
                           # evaluate all rules into model portfolios and 
                           # reconcile them into single model portfolio
                           collected = NULL
                           for(r in ls_rules()) {
                             r = get(r)
                             s = extract.signal(r)
                             p = model.portfolio(s, r$signal.value)
                             p[, check.state:=r$check.state]
                             collected <- if (is.null(collected)) p
                             else
                               .rbind.data.table(collected, p, use.names=TRUE)
                           }
                           
                           setkeyv(collected, c("Instrument", "Date"))
                         # setkeyv(collected, c("Date", "Instrument")) # TODO
                           
                           return(collected)
                         }
                       )
)