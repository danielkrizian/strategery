#' @include Queue.R
OMS <- setRefClass("OMS",
                   fields=list(
                     events="Queue",
                     orders="data.table"
                   ),
                   methods = list(
                     sendOrders = function(){
                       
                     }
                   )
)