
Analysis <- setRefClass("Analysis" # 2 types: portfolio, or asset-level analysis
                         , contains=c("Assets", "Portfolio")
                        , fields = list(name="character"

                         )
                         , methods = list()
)

#' Construct 
assets <- function(x) {
  
  class(out) <- c("assets", class(out))
}

performance <- function(x) {
  cols <- c("Return"="Return", "Performance"="Return")
  Return <- c("Performance", "RoR","")
  class(out) <- c("performance", class(out))
}
