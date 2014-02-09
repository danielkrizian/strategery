parameter <- function(default, set) {
  
#   if(!exists(.parameters))
#     .parameters <- new.env()
  
  if(missing(set))
    set <- NULL
  
  p <- list(default=default, set=set)

  structure(p, class="parameter")
}

is.parameter <- function(x) {
  attr(x,"class")=="parameter"
}