# Express size as shares, equitypct. 
# Attach these objects to the rule object with %someaction% operator

# SetPositionSize(1, method="Shares")
# Value (=1) - dollar value of size (as in previous versions)
# PercentOfEquity (=2) - size expressed as percent of portfolio-level equity (size must be from ..100 (for regular accounts) or .1000 for margin accounts)
# Shares (=4) - size expressed in shares/contracts (size must be > 0 )
# PercentOfPosition (=3) - size expressed as percent of currently open position (for SCALING IN and SCALING OUT ONLY)
# NoChange (=0) - don't change previously set size for given bar

#' @method print position_size
#' @S3method print position_size
print.position_size <- function(x, ...){
  out = paste0("<size> ", eval(x) )
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}

#' @export
shares <- function(qty) {
  structure(.Data=enquote(substitute(qty)), class=c("share","position_size"))
}

#' @export
equitypct <- function(pct) {
  structure(.Data=pct, class=c("weight","position_size"))
}

valid.size <- function(base, side, init.pos=0) {
  buy
  
  # 1. Sell, Short orders must have negative quantities
  size <- -1 * (sell | short) + 1 * (buy | cover)
  
  # 2. Can't enter negative (positive) positions after "Sell" ("Cover").
  #cumsum, validates and add back invalid amounts
  repeat {
    pos.target = base + cumsum(size) # try
    invalid = (pos.target < 0 & sell) | (pos.target > 0 & cover) # check
    if(!any(invalid))
      break
    else
      pos.target = pos.target - size[invalid][1] # correct/add back first
  }
  
  pos.target = pos.target - pos.target[correction][1] # correct/add back first cancelled order
}

#o[, .SD[1][Side=="Sell" | Side=="Cover"], by=Instrument]

#### Experimental/past attempts ####

# PositionSize <- 1 # supports either a numeric, data.table or rule_sizing object 
# # specified as PositionSize <- rule.sizing( psShares(1), input=OHLCV)
# Execution <- "MOC"

##### Collect Signals  ####


rule.execution <- function(call, data)  {
  # experimental
  structure(list(call=call, data=data)
            , class="rule_execution")
}


order_book <- function(){
  
  c("Instrument", "Date","Qty", "Qty.Format", "Side")
  
  #     "Type", "Price","Threshold", "Status", 
  #     "StatusTime", "Prefer", "Set", "Fees", 
  #     "Rule", "Time.In.Force")
  
  
  structure(.Data, class="order_book")
}


rule.sizing <- function(call, data)  {
  # experimental
  structure(list(call=call, data=data)
            , class="rule_sizing")
}

#### Orders-Portfolio interaction: validity, position sizing ####

# if(is.numeric(PositionSize)){
#   orders[,Size:=PositionSize]
# }

# if(is.numeric(Execution)) {
#   o[, Algo:=Execution]
# }

