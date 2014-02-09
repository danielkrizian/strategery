style.fit <- function(R.fund, R.style, model=FALSE, method = c("constrained", "unconstrained", "normalized"), leverage = FALSE, selection = c("none", "AIC"), ...) 
  {
    # INPUTS
    # R.fund   Vector of a fund return time series
    # R.style   Matrix of a style return time series
    # 
    # 
    # OUTPUTS
    # weights   Vector of optimal style index weights
    # @ todo: TE  Tracking error between the calc'd and actual fund
    # @ todo: Fp  Vector of calculated fund time series
    # @ todo: R^2  Coefficient of determination
    
    # 
    # To implement style analysis as described in:
    # http://www.stanford.edu/~wfsharpe/art/sa/sa.htm
    
    method = method[1]
    selection = selection[1]
    
    # Check to see if the required libraries are loaded
    #     if(!require("quadprog", quietly=TRUE))
    #         stop("package", sQuote("quadprog"), "is needed.  Stopping")
    
    R.fund = checkData(R.fund)
    R.style = checkData(R.style)
    
    # @todo: Missing data is not allowed, use = "pairwise.complete.obs" ?
    style.rows = dim(R.style)[1]
    style.cols = dim(R.style)[2]
    fund.rows = dim(R.fund)[1]
    fund.cols = dim(R.fund)[2]
    
    style.colnames = colnames(R.style)
    
    for(fund.col in 1:fund.cols){
      if(method == "constrained"){
        column.result = style.QPfit(R.fund = R.fund[,fund.col,drop=FALSE], R.style = R.style, leverage = leverage)
        if(fund.col == 1){
          result.weights = column.result$weights
          result.R2 = column.result$R.squared
          result.adjR2 = column.result$adj.R.squared
        }
        else{
          result.weights = cbind(result.weights, column.result$weights)
          result.R2 = cbind(result.R2, column.result$R.squared)
          result.adjR2 = cbind(result.adjR2, column.result$adj.R.squared)  
        }
      }
      else if(method == "unconstrained" | method == "normalized"){
        #      A formula has an implied intercept term.  See 'formula' for more details
        #      of allowed formulae.
        
        #      To remove the intercept term: 'y ~ x - 1' is a line through the
        #      origin.  A model with no intercept can be also specified as 'y ~ x
        #      + 0' or 'y ~ 0 + x'.
        
        column.lm = lm(R.fund[, fund.col] ~ 0 + ., data = R.style)
        if (selection == "AIC") { # @todo: add "BIC" case, where k = log(n) and n is style.cols?
          # @todo: support multiple colums
          column.result = step(column.lm) # find the model with minimum AIC value
          if(fund.col == 1 )
            column.weights=data.frame(matrix(rep(0,length(style.colnames)*fund.cols), nrow = length(style.colnames), ncol = fund.cols),row.names=style.colnames)
          column.coef = as.data.frame(coef(column.result))
          
          if(length(coef(column.result))>0){
            row.loc = match(rownames(column.coef), rownames(column.weights))
            for(i in 1:length(row.loc)) column.weights[row.loc[i],fund.col]=column.coef[i,1]
          }
        }
        else {
          column.result = column.lm
          column.weights = as.data.frame(coef(column.lm))
        }
        #             column.weights = as.data.frame(coef(column.result)[-1])
        #             colnames(column.weights)= style.colnames[fund.col]
        rownames(column.weights) = colnames(R.style)
        colnames(column.weights) = colnames(R.fund)[fund.col]
        
        R2 = as.data.frame(summary(column.result)$r.squared)
        adjR2 = as.data.frame(summary(column.result)$adj.r.squared)
        colnames(R2) = colnames(R.fund)[fund.col]
        colnames(adjR2) = colnames(R.fund)[fund.col] 
        rownames(R2) = "R-squared"
        rownames(adjR2) = "Adj R-squared"
        
        if(method == "normalized") {
          column.weights = column.weights/sum(column.weights)
        }
        if(fund.col == 1){
          result.weights = column.weights
          result.R2 = R2
          result.adjR2 = adjR2
        }
        else{
          result.weights = cbind(result.weights, column.weights)
          result.R2 = cbind(result.R2, R2)
          result.adjR2 = cbind(result.adjR2, adjR2)
        }
      }
      else stop("Method is mis-specified.  Select from \"constrained\", \"unconstrained\", or  \"normalized\"")
    }
    result = list(weights = result.weights, R.squared = result.R2, adj.R.squared = result.adjR2 )
    
    return(result)
    
  }

style.QPfit <- function(R.fund, R.style, model=FALSE, leverage = FALSE, ...) 
  {
    # INPUTS
    # R.fund   Vector of a fund return time series
    # R.style   Matrix of a style return time series
    # 
    # 
    # OUTPUTS
    # weights   Vector of optimal style index weights
    # @ todo: TE  Tracking error between the calc'd and actual fund
    # @ todo: Fp  Vector of calculated fund time series
    # @ todo: R^2  Coefficient of determination
    #
    # 
    # w_i   Style weights
    # V   Variance-covariance matrix of style index matrix
    # C   Vector of covariances between the style index and the fund
    # e   Vector of 1's
    # n   Number of style indexes
    # 
    # To implement style analysis as described in:
    # http://www.stanford.edu/~wfsharpe/art/sa/sa.htm
    # here's what we're trying to do:
    # min VAR(R.f - SUM[w_i * R.s_i]) = min VAR(F - w*S)
    #   s.t. SUM[w_i] = 1; w_i > 0
    # 
    # Remembering that VAR(aX + bY) = a^2 VAR(X) + b^2 VAR(Y) + 2ab COV(X,Y), 
    # we can refactor our problem as:
    # 
    # = VAR(R.f) + w'*V*w - 2*w'*COV(R.f,R.s)
    # 
    # drop VAR[R.f] as it isn't a function of weights, multiply by 1/2:
    # 
    # = min (1/2) w'*V*w - C'w
    #   s.t. w'*e = 1, w_i > 0
    # 
    # Now, map that to the inputs of solve.QP, which is specified as:
    # min(-d' b + 1/2 b' D b) with the constraints A' b >= b_0
    # 
    # so:
    # b is the weight vector,
    # D is the variance-covariance matrix of the styles
    # d is the covariance vector between the fund and the styles
    #
    
    # Check to see if the required libraries are loaded
    if(!require("quadprog", quietly=TRUE))
      stop("package", sQuote("quadprog"), "is needed.  Stopping")
    
    R.fund = checkData(R.fund[,1,drop=FALSE], method = "zoo")
    R.style = checkData(R.style, method = "zoo")
    
    # @todo: Missing data is not allowed, use = "pairwise.complete.obs" ?
    style.rows = dim(R.style)[1]
    style.cols = dim(R.style)[2]
    
    # Calculate D and d
    Dmat = cov(R.style, use = "pairwise.complete.obs")
    dvec = cov(R.fund, R.style, use = "pairwise.complete.obs")
    
    # To specify A' b >= b_0, we create an nxn matrix Amat and the constraints 
    # vector b0.  First we tackle Amat.  
    
    # If we do not allow leverage, the first constraint listed above is 
    # SUM[w_i] = 1.  The left side of the constraint is expressed as a vector 
    # of 1's:
    if(!leverage)
      a1 = rep(1, style.cols)
    
    # In b0, which represents the right side of the equation, this vector is 
    # paired with the value '1'.
    
    # The second constraint sets the lower bound of the weights to zero.  First
    # we set up an identity matrix.  
    a2 = matrix(0, style.cols, style.cols)
    diag(a2) = 1
    
    # It's paired in b0 with a vector of lower bounds set to zeros:
    w.min = rep(0, style.cols)
    
    # Construct A from the two components a1 and a2
    # Construct b_0
    if(!leverage){
      Amat = t(rbind(a1, a2))
      b0 = c(1, w.min)
    }
    else {
      Amat = t(a2)
      b0 = w.min
    }
    
    # This is where 'meq' comes in.  The ?solve.QP page says:
    #     meq: the first 'meq' constraints are treated as equality
    #     constraints, all further as inequality constraints (defaults
    #     to 0).
    # I think that the way to interpret this is: if it is set to a value 'q' <= n,
    # the ordered constraints numbered less than or equal to 'q' are treated as an 
    # equality constraint.  In this case, we only want to first constraint to be
    # treated as an equality, so that the weights would sum to exactly 1.  So we
    # set meq = 1.
    
    # With 'meq' set to 1, the second constraint (a2) is treated as an inequality,
    # so each weight is constrainted to be greater than or equal to zero.
    optimal <- solve.QP(Dmat, dvec, Amat, bvec=b0, meq=1)
    
    weights = as.data.frame(optimal$solution)
    rownames(weights) = colnames(R.style)
    colnames(weights) = colnames(R.fund)[1]
    
    # Calculate metrics for the quality of the fit
    R.fitted = rowSums(R.style*matrix(rep(t(weights),style.rows),byrow=TRUE,ncol=style.cols))
    R.nonfactor = R.fund - R.fitted
    
    R.squared = as.data.frame(1 - (var(R.nonfactor, na.rm=TRUE)/var(R.fund, na.rm=TRUE)))
    #     adj.R.squared = as.data.frame(1 - (1 - R.squared)*(style.rows - 1)/(style.rows - style.cols - 1))
    
    rownames(R.squared) = "R-squared"
    #     rownames(adj.R.squared) = "Adj R-squared"
    
    if(model) 
      result = optimal
    else 
      result = list(weights = weights, R.squared = R.squared) #, adj.R.squared = adj.R.squared )
    
    # @todo: retain the labels on the weights
    # @todo: add other values to output, e.g., 
    #    result <- list(weights = optimal$solution, R.squared = , tracking.error = )
    
    return(result)
    
    
    # EXAMPLE:
    # > head(R.fund)
    #          SP500.TR
    # Jan 1996   0.0340
    # Feb 1996   0.0093
    # Mar 1996   0.0096
    # Apr 1996   0.0147
    # May 1996   0.0258
    # Jun 1996   0.0038
    # > head(R.style)
    #          Russell.1000.Growth Russell.1000.Value
    # Jan 1996              0.0335             0.0312
    # Feb 1996              0.0183             0.0076
    # Mar 1996              0.0013             0.0170
    # Apr 1996              0.0263             0.0038
    # May 1996              0.0349             0.0125
    # Jun 1996              0.0014             0.0008
    # > style.QPfit(R.fund, R.style)
    # [1] 0.5047724 0.4952276
    # > style.QPfit(R.fund, R.style, all=T)
    # $solution
    # [1] 0.5047724 0.4952276
    # 
    # $value
    # [1] -0.0008888153
    # 
    # $unconstrainted.solution
    # [1] 0.5040111 0.4815228
    # 
    # $iterations
    # [1] 2 0
    # 
    # $iact
    # [1] 1
    # 
}

table.RollingStyle <-
  function (R.fund, R.style, method = c("constrained","unconstrained","normalized"), leverage = FALSE, width = 12, ...)
  { # @author Peter Carl
    
    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time
    
    # R-Squared could deliver adjusted R-Squared if we wanted
    
    # FUNCTION:
    
    # Transform input data to a data frame
    R.fund = checkData(R.fund[,1,drop=FALSE])
    R.style = checkData(R.style)
    
    method = method[1]
    
    # Get dimensions and labels
    columns.fund = ncol(R.fund)
    columns.style = ncol(R.style)
    columnnames.fund = colnames(R.fund)
    columnnames.style = colnames(R.style)
    
    
    # Calculate
    merged.assets = na.omit(merge(R.fund, R.style))
    
    result = xts:::rollapply.xts(merged.assets, FUN= function(x, method, leverage) {t(style.fit(R.fund = x[,1,drop=FALSE], R.style = x[,-1,drop=FALSE], method = method, leverage = leverage)$weights)}, width = width, method = method, leverage = leverage, by = 1, by.column = FALSE, na.pad = FALSE, align = "right")
    
    colnames(result) = columnnames.style
    return(result)
  }

chart.RollingStyle <-
  function (R.fund, R.style, method = c("constrained","unconstrained","normalized"), leverage = FALSE, width = 12, main = NULL, space = 0, ...)
  { # @author Peter Carl
    
    result<-table.RollingStyle(R.fund=R.fund, R.style=R.style, method=method,leverage=leverage,width=width)
    
    if (is.null(main)){
      freq = periodicity(R.fund)
      
      switch(freq$scale,
             minute = {freq.lab = "minute"},
             hourly = {freq.lab = "hour"},
             daily = {freq.lab = "day"},
             weekly = {freq.lab = "week"},
             monthly = {freq.lab = "month"},
             quarterly = {freq.lab = "quarter"},
             yearly = {freq.lab = "year"}
      )
      
      main = paste(colnames(R.fund)[1]," Rolling ", width ,"-",freq.lab," Style Weights", sep="")
    }
    
    chart.StackedBar(result, main = main, space = space, ...)
    
  }

#' calculate and display effective style weights
#' 
#' Functions that calculate effective style weights and display the results in
#' a bar chart.  \code{chart.Style} calculates and displays style weights
#' calculated over a single period.  \code{chart.RollingStyle} calculates and
#' displays those weights in rolling windows through time.  \code{style.fit}
#' manages the calculation of the weights by method.  \code{style.QPfit}
#' calculates the specific constraint case that requires quadratic programming.
#' 
#' These functions calculate style weights using an asset class style model as
#' described in detail in Sharpe (1992).  The use of quadratic programming to
#' determine a fund's exposures to the changes in returns of major asset
#' classes is usually refered to as "style analysis".
#' 
#' The "unconstrained" method implements a simple factor model for style
#' analysis, as in: \deqn{Ri = bi1*F1+bi2*F2+...+bin*Fn+ei}{R_i =
#' b_{i1}F_1+b_{i2}F_2+\dots+b_{in}F_n +e_i} where \eqn{Ri}{R_i} represents the
#' return on asset i, \eqn{Fj}{F_j} represents each factor, and \eqn{ei}{e_i}
#' represents the "non-factor" component of the return on i.  This is simply a
#' multiple regression analysis with fund returns as the dependent variable and
#' asset class returns as the independent variables.  The resulting slope
#' coefficients are then interpreted as the fund's historic exposures to asset
#' class returns.  In this case, coefficients do not sum to 1.
#' 
#' The "normalized" method reports the results of a multiple regression
#' analysis similar to the first, but with one constraint: the coefficients are
#' required to add to 1.  Coefficients may be negative, indicating short
#' exposures. To enforce the constraint, coefficients are normalized.
#' 
#' The "constrained" method includes the constraint that the coefficients sum
#' to 1, but adds that the coefficients must lie between 0 and 1. These
#' inequality constraints require a quadratic programming algorithm using
#' \code{\link[quadprog]{solve.QP}} from the 'quadprog' package, and the
#' implementation is discussed under \code{\link{style.QPfit}}.  If set to
#' TRUE, "leverage" allows the sum of the coefficients to exceed 1.
#' 
#' According to Sharpe (1992), the calculation for the constrained case is
#' represented as: \deqn{min var(Rf - sum[wi * R.si]) = min var(F - w*S)}{min
#' \sigma(R_f - \sum{w_i * R_s_i}) = min \sigma(F - w*S)} \deqn{s.t. sum[wi] =
#' 1; wi > 0}{ s.t. \sum{w_i} = 1; w_i > 0}
#' 
#' Remembering that:
#' 
#' \deqn{\sigma(aX + bY) = a^2 \sigma(X) + b^2 \sigma(Y) + 2ab cov(X,Y) =
#' \sigma(R.f) + w'*V*w - 2*w'*cov(R.f,R.s)}
#' 
#' we can drop \eqn{var(Rf)}{\sigma(R_f)} as it isn't a function of weights,
#' multiply both sides by 1/2:
#' 
#' \deqn{= min (1/2) w'*V*w - C'w}{= min (1/2) w'*V*w - C'w} \deqn{ s.t. w'*e =
#' 1, w_i > 0}{ s.t. w'*e = 1, w_i > 0}
#' 
#' Which allows us to use \code{\link[quadprog]{solve.QP}}, which is specified
#' as: \deqn{min(-d' b + 1/2 b' D b)}{min(-d' b + 1/2 b' D b)} and the
#' constraints \deqn{ A' b >= b.0 }{ A' b >= b_0 }
#' 
#' so: b is the weight vector, D is the variance-covariance matrix of the
#' styles d is the covariance vector between the fund and the styles
#' 
#' The chart functions then provide a graphical summary of the results.  The
#' underlying function, \code{\link{style.fit}}, provides the outputs of the
#' analysis and more information about fit, including an R-squared value.
#' 
#' Styles identified in this analysis may be interpreted as an average of
#' potentially changing exposures over the period covered.  The function
#' \code{\link{chart.RollingStyle}} may be useful for examining the behavior of
#' a manager's average exposures to asset classes over time, using a
#' rolling-window analysis.
#' 
#' The chart functions plot a column chart or stacked column chart of the
#' resulting style weights to the current device.  Both \code{style.fit} and
#' \code{style.QPfit} produce a list of data frames containing 'weights' and
#' 'R.squared' results.  If 'model' = TRUE in \code{style.QPfit}, the full
#' result set is shown from the output of \code{solve.QP}.
#' 
#' @aliases chart.Style chart.RollingStyle table.RollingStyle style.fit
#' style.QPfit
#' @param R.fund matrix, data frame, or zoo object with fund returns to be
#' analyzed
#' @param R.style matrix, data frame, or zoo object with style index returns.
#' Data object must be of the same length and time-aligned with R.fund
#' @param method specify the method of calculation of style weights as
#' "constrained", "unconstrained", or "normalized".  For more information, see
#' \code{\link{style.fit}}
#' @param leverage logical, defaults to 'FALSE'.  If 'TRUE', the calculation of
#' weights assumes that leverage may be used.  For more information, see
#' \code{\link{style.fit}}
#' @param model logical. If 'model' = TRUE in \code{\link{style.QPfit}}, the
#' full result set is shown from the output of \code{solve.QP}.
#' @param selection either "none" (default) or "AIC".  If "AIC", then the
#' function uses a stepwise regression to identify find the model with minimum
#' AIC value.  See \code{\link{step}} for more detail.
#' @param unstacked logical.  If set to 'TRUE' \emph{and} only one row of data
#' is submitted in 'w', then the chart creates a normal column chart.  If more
#' than one row is submitted, then this is ignored.  See examples below.
#' @param space the amount of space (as a fraction of the average bar width)
#' left before each bar, as in \code{\link{barplot}}. Default for
#' \code{chart.RollingStyle} is 0; for \code{chart.Style} the default is 0.2.
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param width number of periods or window to apply rolling style analysis
#' over
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param \dots for the charting functions, these are arguments to be passed to
#' \code{\link{barplot}}. These can include further arguments (such as 'axes',
#' 'asp' and 'main') and graphical parameters (see 'par') which are passed to
#' 'plot.window()', 'title()' and 'axis'. For the calculation functions, these
#' are ignored.
#' @note None of the functions \code{chart.Style}, \code{style.fit}, and
#' \code{style.QPfit} make any attempt to align the two input data series. The
#' \code{chart.RollingStyle}, on the other hand, does merge the two series and
#' manages the calculation over common periods.
#' @author Peter Carl
#' @seealso \code{\link{barplot}}, \code{\link{par}}
#' @references Sharpe, W. Asset Allocation: Management Style and Performance
#' Measurement Journal of Portfolio Management, 1992, 7-19.  See \url{
#' http://www.stanford.edu/~wfsharpe/art/sa/sa.htm}
#' @keywords ts multivariate hplot
#' @examples
#' 
#' data(edhec)
#' data(managers)
#' style.fit(as.data.frame(managers[97:132,2,drop=FALSE]),as.data.frame(edhec[85:120,]), method="constrained", leverage=FALSE)
#' chart.Style(managers[97:132,2,drop=FALSE],edhec[85:120,], method="constrained", leverage=FALSE, unstack=TRUE, las=3)
#' chart.RollingStyle(managers[,2,drop=FALSE],edhec[,1:11], method="constrained", leverage=FALSE, width=36, cex.legend = .7, colorset=rainbow12equal, las=1)
#' 
chart.Style <-
  function (R.fund, R.style, method = c("constrained", "unconstrained", "normalized"), leverage = FALSE, main = NULL, ylim = NULL, unstacked=TRUE, ...)
  { # @author Peter Carl
    
    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time
    
    # R-Squared could deliver adjusted R-Squared if we wanted
    
    # FUNCTION:
    
    # Transform input data to a data frame
    R.fund = checkData(R.fund)
    R.style = checkData(R.style)
    method = method[1]
    
    # Calculate
    result = style.fit(R.fund, R.style, method = method, leverage = leverage)
    weights = t(as.matrix(result$weights))
    
    if(is.null(main))
      main = paste(colnames(R.fund)[1] ," Style Weights", sep="")
    
    if(is.null(ylim))
      if(method == "constrained" & leverage == FALSE) ylim = c(0,1)
    else ylim = NULL
    
    chart.StackedBar(weights, main = main, ylim = ylim, unstacked = unstacked, ...)
    #     barplot(weights, main = main, ylim = ylim, ...)
    
  }
