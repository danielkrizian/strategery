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
                         model.portfolio = function(signal, sizing) {
                           if(is.numeric(sizing))
                             signal[, Pos:=sizing]
                         },
                         signals = function(){
                           # evaluate all rules into model portfolios and 
                           # reconcile them into single model portfolio
                           mp = NULL
                           for(r in ls_rules()) {
                             r = get(r)
                             s = extract.signal(r)
                             p = model.portfolio(s, r$size)
                             mp <- if (is.null(mp)) p
                             else
                               .rbind.data.table(mp, p, use.names=TRUE)
                           }
                           
                           setkeyv(mp, names(mp)[-length(names(mp))])
                           
                           if(nrow(mp[duplicated(mp)])) {
                             print("Ambiguous position signals found at: ")
                             print(mp[duplicated(mp)])
                             browser()
                           }
                           return(mp)
                         }
                       )
)