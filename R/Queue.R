#' resources: 
#' http://www.exegetic.biz/blog/2013/11/implementing-a-queue-as-a-reference-class/
#' http://www.exegetic.biz/blog/2013/11/deriving-a-priority-queue/
#' https://github.com/rstudio/shiny/blob/master/R/priorityqueue.R
Queue <- setRefClass(Class = "Queue",
                     fields = list(
                       name = "character",
                       data = "list"
                     ),
                     methods = list(
                       size = function() {
                         'Returns the number of items in the queue.'
                         return(length(data))
                       },
                       #
                       push = function(item) {
                         'Inserts element at back of the queue.'
                         data[[size()+1]] <<- item
                       },
                       #
                       pop = function() {
                         'Removes and returns head of queue (or raises error if queue is empty).'
                         if (size() == 0) stop("queue is empty!")
                         value <- data[[1]]
                         data[[1]] <<- NULL
                         value
                       },
                       #
                       poll = function() {
                         'Removes and returns head of queue (or NULL if queue is empty).'
                         if (size() == 0) return(NULL)
                         else pop()
                       },
                       #
                       peek = function(pos = c(1)) {
                         'Returns (but does not remove) specified positions in queue (or NULL if any one of them is not available).'
                         if (size() < max(pos)) return(NULL)
                         #
                         if (length(pos) == 1) return(data[[pos]])
                         else return(data[pos])
                       },
                       initialize=function(...) {
                         callSuper(...)
                         #
                         # Initialise fields here (place holder)...
                         #
                         .self
                       }
                     )
)

PriorityQueue <- setRefClass("PriorityQueue",
                             contains = "Queue",
                             fields = list(
                               priorities = "numeric"
                             ),
                             methods = list(
                               push = function(item, priority) {
                                 'Inserts element into the queue, reordering according to priority.'
                                 callSuper(item)
                                 priorities <<- c(priorities, priority)
                                 #
                                 order = order(priorities, decreasing = TRUE, partial = size():1)
                                 #
                                 data <<- data[order]
                                 priorities <<- priorities[order]
                               },
                               #
                               pop = function() {
                                 'Removes and returns head of queue (or raises error if queue is empty).'
                                 if (size() == 0) stop("queue is empty!")
                                 priorities <<- priorities[-1]
                                 callSuper()
                               })
)